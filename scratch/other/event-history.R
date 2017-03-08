# First pass at using events to estimate historical backlogs

source("R/vacolsConnect.R")
source("R/events.R")

library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

logical_cases <- dbGetQuery(con, "
  select BFCORLID, BFDNOD, BFKEY, BFAC, BFDC, BFDDEC, BFDPDCN

  from BRIEFF

  join

  (
    select distinct
      BFCORLID as DISTBFCORLID,
      BFDNOD as DISTBFDNOD

    from BRIEFF

    where BFDDEC is null or BFDDEC > date '2013-10-01'
  )

  on BFCORLID = DISTBFCORLID and BFDNOD = DISTBFDNOD
")

logical_cases %<>%
  filter(BFAC == '1') %>%
  mutate(case_id = row_number()) %>%
  select(BFKEY, case_id) %>%
  right_join(logical_cases, by = c("BFKEY"))

logical_cases.na <- nrow(logical_cases)

while(logical_cases.na - sum(is.na(logical_cases$case_id)) > 0) {
  logical_cases.na <- sum(is.na(logical_cases$case_id))

  logical_cases %<>%
    filter(!is.na(case_id)) %>%
    select(BFCORLID, BFDNOD, BFDDEC, case_id) %>%
    right_join(logical_cases, by = c("BFCORLID", "BFDNOD", "BFDDEC" = "BFDPDCN")) %>%
    mutate(case_id = ifelse(is.na(case_id.y), case_id.x, case_id.y), BFDPDCN = BFDDEC, BFDDEC = BFDDEC.y) %>%
    select(-case_id.x, -case_id.y, -BFDDEC.y) %>%
    unique()
}

logical_cases %<>%
  select(BFKEY, case_id) %>%
  group_by(BFKEY) %>%
  filter(n() == 1, !is.na(case_id)) %>%
  ungroup()

events <- event_all(con, where = "BFDNOD > date '1990-01-01'", join = "
  (
    select distinct
      BFCORLID as DISTBFCORLID,
      BFDNOD as DISTBFDNOD

    from BRIEFF

    where BFDDEC is null or BFDDEC > date '2013-10-01'
  )

  on BFCORLID = DISTBFCORLID and BFDNOD = DISTBFDNOD
")

cleaned_events <- events %>%

  inner_join(logical_cases, by = c("BFKEY")) %>%

  group_by(BFKEY) %>%

  # Remove duplicate events from substitution appeals
  arrange(desc(DATE)) %>%
  mutate(sub = cumsum(EVENT_TYPE == "SUBSTITUTION")) %>%
  filter(sub == 0 | EVENT_TYPE == "SUBSTITUTION") %>%
  select(-sub) %>%

  # Remove before-hearing CASE_REVIEW and ACTIVATION events
  mutate(had_hearing = cumsum(EVENT_TYPE == "HEARING" | EVENT_TYPE == "HEARING_EXCEPTION"),
         had_post_decision_hearing = cumsum(EVENT_TYPE == "DECISION" & had_hearing > 0)) %>%
  filter((EVENT_TYPE != "CASE_REVIEW" & EVENT_TYPE != "ACTIVATION") | had_hearing - had_post_decision_hearing == 0) %>%
  select(-had_hearing, -had_post_decision_hearing) %>%

  # Remove after-hearing CASE_REVIEW and ACTIVATION events
  arrange(DATE) %>%
  mutate(had_hearing = cumsum(EVENT_TYPE == "HEARING" | EVENT_TYPE == "HEARING_EXCEPTION")) %>%
  filter((EVENT_TYPE != "CASE_REVIEW" & EVENT_TYPE != "ACTIVATION") | had_hearing == 0) %>%
  select(-had_hearing) %>%

  unique() %>%

  ungroup() %>%

  # Restore order
  arrange(BFCORLID, case_id, DATE)

gp_events <- cleaned_events %>%
  filter(EVENT_TYPE %in% c(
    "NOD", "CAVC", "SOC", "FORM9", "SSOC", "CERTIFICATION", "REMAND_RETURN", "ACTIVATION", "CASE_REVIEW", "HEARING_EXCEPTION",
    "HEARING", "ASSIGNMENT", "SIGNED_DECISION", "OUTCODING", "WITHDRAWN", "GRANT"
  ))

not_remanded_decisions <- dbGetQuery(con, "select BFKEY from BRIEFF where BFDDEC > date '2013-10-01' and ((BFDC = '1' and VACOLS.ISSUE_CNT_REMAND(BFKEY) = 0) or BFDC = '4')") %>%
  mutate(not_remanded = TRUE)

gp_events <- left_join(gp_events, not_remanded_decisions, by = c("BFKEY"))
gp_events$EVENT_TYPE[gp_events$not_remanded & gp_events$EVENT_TYPE == "OUTCODING"] <- "FINAL_OUTCODING"

gp_events %<>%
  select(-not_remanded) %>%
  mutate(
    DATE = as.Date(DATE),
    EVENT_TYPE = factor(EVENT_TYPE, levels = c(
      "NOD", "CAVC", "SOC", "FORM9", "SSOC", "CERTIFICATION", "REMAND_RETURN", "ACTIVATION", "CASE_REVIEW", "HEARING_EXCEPTION",
      "HEARING", "ASSIGNMENT", "SIGNED_DECISION", "OUTCODING", "FINAL_OUTCODING", "WITHDRAWN", "GRANT"
    ))
  ) %>%
  arrange(BFCORLID, case_id, DATE, EVENT_TYPE) %>%
  group_by(case_id) %>%
  mutate(
    PREV_EVENT = lag(EVENT_TYPE),
    NEXT_EVENT = lead(EVENT_TYPE)
  ) %>%
  ungroup()


gp_events$step <- NA
gp_events$outcome <- NA

gp_events$step[is.na(gp_events$PREV_EVENT) & gp_events$EVENT_TYPE == "NOD"] <- "NOD"
gp_events$outcome[is.na(gp_events$PREV_EVENT) & gp_events$EVENT_TYPE == "NOD"] <- "NOD"

gp_events$step[gp_events$PREV_EVENT == "NOD" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "POST_NOD_WITHDRAWAL"
gp_events$outcome[gp_events$PREV_EVENT == "NOD" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "WITHDRAWN"

gp_events$step[gp_events$PREV_EVENT == "NOD" & gp_events$EVENT_TYPE == "SOC"] <- "SOC"
gp_events$outcome[gp_events$PREV_EVENT == "NOD" & gp_events$EVENT_TYPE == "SOC"] <- "SOC"

gp_events$step[gp_events$PREV_EVENT == "NOD" & gp_events$EVENT_TYPE == "GRANT"] <- "SOC"
gp_events$outcome[gp_events$PREV_EVENT == "NOD" & gp_events$EVENT_TYPE == "GRANT"] <- "GRANT"

gp_events$step[gp_events$PREV_EVENT == "SOC" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "POST_SOC_WITHDRAWAL"
gp_events$outcome[gp_events$PREV_EVENT == "SOC" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "WITHDRAWN"

gp_events$step[gp_events$PREV_EVENT %in% c("SOC", "SSOC") & gp_events$EVENT_TYPE == "FORM9"] <- "FORM9"
gp_events$outcome[gp_events$PREV_EVENT %in% c("SOC", "SSOC") & gp_events$EVENT_TYPE == "FORM9"] <- "FORM9"

gp_events$step[gp_events$PREV_EVENT %in% c("SOC", "FORM9", "SSOC", "CERTIFICATION", "OUTCODING") & gp_events$EVENT_TYPE == "SSOC"] <- "SSOC"
gp_events$outcome[gp_events$PREV_EVENT %in% c("SOC", "FORM9", "SSOC", "CERTIFICATION", "OUTCODING") & gp_events$EVENT_TYPE == "SSOC"] <- "SSOC"

gp_events$step[gp_events$PREV_EVENT %in% c("SOC", "FORM9", "SSOC", "CERTIFICATION", "OUTCODING") & gp_events$EVENT_TYPE == "GRANT"] <- "SSOC"
gp_events$outcome[gp_events$PREV_EVENT %in% c("SOC", "FORM9", "SSOC", "CERTIFICATION", "OUTCODING") & gp_events$EVENT_TYPE == "GRANT"] <- "GRANT"

gp_events$step[gp_events$PREV_EVENT == "SSOC" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "POST_SSOC_WITHDRAWAL"
gp_events$outcome[gp_events$PREV_EVENT == "SSOC" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "WITHDRAWN"

gp_events$step[gp_events$PREV_EVENT == "FORM9" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "POST_FORM9_WITHDRAWAL"
gp_events$outcome[gp_events$PREV_EVENT == "FORM9" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "WITHDRAWN"

gp_events$step[gp_events$PREV_EVENT %in% c("SSOC", "FORM9") & gp_events$EVENT_TYPE == "CERTIFICATION"] <- "CERTIFICATION"
gp_events$outcome[gp_events$PREV_EVENT %in% c("SSOC", "FORM9") & gp_events$EVENT_TYPE == "CERTIFICATION"] <- "CERTIFICATION"

gp_events$step[gp_events$PREV_EVENT == "CERTIFICATION" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "POST_CERTIFICATION_WITHDRAWAL"
gp_events$outcome[gp_events$PREV_EVENT == "CERTIFICATION" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "WITHDRAWN"

gp_events$step[gp_events$PREV_EVENT %in% c("OUTCODING", "SSOC", "HEARING", "HEARING_EXCEPTION") & gp_events$EVENT_TYPE == "REMAND_RETURN"] <- "REMAND_RETURN"
gp_events$outcome[gp_events$PREV_EVENT %in% c("OUTCODING", "SSOC", "HEARING", "HEARING_EXCEPTION") & gp_events$EVENT_TYPE == "REMAND_RETURN"] <- "REMAND_RETURN"

gp_events$step[gp_events$PREV_EVENT == "REMAND_RETURN" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "POST_REMAND_RETURN_WITHDRAWAL"
gp_events$outcome[gp_events$PREV_EVENT == "REMAND_RETURN" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "WITHDRAWN"

gp_events$step[gp_events$PREV_EVENT %in% c("CERTIFICATION", "SSOC", "REMAND_RETURN", "CAVC") & gp_events$EVENT_TYPE == "ACTIVATION"] <- "ACTIVATION"
gp_events$outcome[gp_events$PREV_EVENT %in% c("CERTIFICATION", "SSOC", "REMAND_RETURN", "CAVC") & gp_events$EVENT_TYPE == "ACTIVATION"] <- "ACTIVATION"

gp_events$step[gp_events$PREV_EVENT == "ACTIVATION" & gp_events$EVENT_TYPE == "CASE_REVIEW"] <- "CASE_REVIEW"
gp_events$outcome[gp_events$PREV_EVENT == "ACTIVATION" & gp_events$EVENT_TYPE == "CASE_REVIEW"] <- "CASE_REVIEW"

gp_events$step[gp_events$PREV_EVENT %in% c("CERTIFICATION", "HEARING_EXCEPTION", "OUTCODING", "SSOC") & gp_events$EVENT_TYPE == "HEARING"] <- "HEARING"
gp_events$outcome[gp_events$PREV_EVENT %in% c("CERTIFICATION", "HEARING_EXCEPTION", "OUTCODING", "SSOC") & gp_events$EVENT_TYPE == "HEARING"] <- "HEARING"

gp_events$step[gp_events$PREV_EVENT %in% c("CERTIFICATION", "HEARING_EXCEPTION") & gp_events$EVENT_TYPE == "HEARING_EXCEPTION"] <- "HEARING"
gp_events$outcome[gp_events$PREV_EVENT %in% c("CERTIFICATION", "HEARING_EXCEPTION") & gp_events$EVENT_TYPE == "HEARING_EXCEPTION"] <- "EXCEPTION"

gp_events$step[gp_events$PREV_EVENT %in% c("HEARING", "HEARING_EXCEPTION", "CASE_REVIEW") & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "POST_ACTIVATION_WITHDRAWAL"
gp_events$outcome[gp_events$PREV_EVENT %in% c("HEARING", "HEARING_EXCEPTION", "CASE_REVIEW") & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "WITHDRAWN"

gp_events$step[gp_events$PREV_EVENT %in% c("HEARING", "HEARING_EXCEPTION", "CASE_REVIEW", "ASSIGNMENT") & gp_events$EVENT_TYPE == "ASSIGNMENT"] <- "ASSIGNMENT"
gp_events$outcome[gp_events$PREV_EVENT %in% c("HEARING", "HEARING_EXCEPTION", "CASE_REVIEW", "ASSIGNMENT") & gp_events$EVENT_TYPE == "ASSIGNMENT"] <- "ASSIGNMENT"

gp_events$step[gp_events$PREV_EVENT == "ASSIGNMENT" & gp_events$EVENT_TYPE == "SIGNED_DECISION"] <- "DECISION"
gp_events$outcome[gp_events$PREV_EVENT == "ASSIGNMENT" & gp_events$EVENT_TYPE == "SIGNED_DECISION"] <- "DECISION"

gp_events$step[gp_events$PREV_EVENT %in% c("SIGNED_DECISION") & gp_events$EVENT_TYPE == "OUTCODING"] <- "OUTCODING"
gp_events$outcome[gp_events$PREV_EVENT %in% c("SIGNED_DECISION") & gp_events$EVENT_TYPE == "OUTCODING"] <- "REMAND"

gp_events$step[gp_events$PREV_EVENT %in% c("SIGNED_DECISION") & gp_events$EVENT_TYPE == "FINAL_OUTCODING"] <- "OUTCODING"
gp_events$outcome[gp_events$PREV_EVENT %in% c("SIGNED_DECISION") & gp_events$EVENT_TYPE == "FINAL_OUTCODING"] <- "FINAL"

gp_events$step[gp_events$PREV_EVENT == "OUTCODING" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "POST_REMAND_WITHDRAWAL"
gp_events$outcome[gp_events$PREV_EVENT == "OUTCODING" & gp_events$EVENT_TYPE == "WITHDRAWN"] <- "WITHDRAWN"

gp_events$step[gp_events$PREV_EVENT == "OUTCODING" & gp_events$EVENT_TYPE == "GRANT"] <- "SSOC"
gp_events$outcome[gp_events$PREV_EVENT == "OUTCODING" & gp_events$EVENT_TYPE == "GRANT"] <- "GRANT"

gp_events$step[gp_events$EVENT_TYPE == "CAVC"] <- "CAVC"
gp_events$outcome[gp_events$EVENT_TYPE == "CAVC"] <- "CAVC"


today <- Sys.Date()

working_events <- gp_events %>%
  group_by(case_id) %>%
  filter(sum(is.na(step)) == 0) %>%
  mutate(post_final_cavc = cumsum(EVENT_TYPE == "CAVC" & PREV_EVENT %in% c("FINAL_OUTCODING", "WITHDRAWN", "GRANT"))) %>%
  ungroup() %>%
  select(-EVENT_TYPE, -PREV_EVENT, -NEXT_EVENT)

working_events$case_id <- working_events %>% group_indices(case_id, post_final_cavc)

working_events %<>%
  select(-post_final_cavc) %>%
  group_by(case_id) %>%
  mutate(
    prev_outcome = lag(outcome),
    next_step = lead(step),
    next_step_date = lead(DATE),
    duration = ifelse(is.na(next_step_date), as.numeric(today - DATE), as.numeric(next_step_date - DATE))
  ) %>%
  ungroup() %>%
  replace_na(list(next_step_date = today))

library(cmprsk)

probs_for_outcome <- function (events, ostr) {
  .outcome_events <- events %>%
    filter(outcome == ostr, next_step != "CAVC", duration >= 0) %>%
    replace_na(list(next_step = "censor"))

  .incidence <- cuminc(.outcome_events$duration, .outcome_events$next_step, cencode = "censor")
  .timepoints <- timepoints(.incidence, seq(0, max(.outcome_events$duration), by = 1))$est

  .remaining <- (.timepoints[,ncol(.timepoints)] - .timepoints) /
    matrix(rep(1 - colSums(.timepoints), nrow(.timepoints)), nrow(.timepoints), byrow = TRUE)

  .df <- .remaining %>%
    as.data.frame() %>%
    mutate(
      outcome = ostr,
      next_step = substring(rownames(.remaining), 3)
    ) %>%
    gather(elapsed, value, -outcome, -next_step) %>%
    mutate(elapsed = as.numeric(elapsed))

  return(.df)
}

next_step_probs <- data.frame(outcome = character(), next_step = character(), elapsed = numeric(), value = numeric(), stringsAsFactors = FALSE)

outcomes <- working_events %>%
  filter(!is.na(next_step), next_step != "CAVC") %>%
  select(outcome) %>%
  unique()

for(ostr in outcomes$outcome) {
  .df <- probs_for_outcome(working_events, ostr)
  next_step_probs <- rbind(next_step_probs, .df)
}

nod_fog <- cleaned_events %>%
  filter(EVENT_TYPE == "NOD" | EVENT_TYPE == "VACOLS", as.Date(APPEAL_DATE) > today - 3 * 365) %>%
  spread(EVENT_TYPE, DATE) %>%
  mutate(elapsed = as.numeric(as.Date(VACOLS) - as.Date(NOD))) %>%
  filter(elapsed >= 0, elapsed <= 60) %>%
  count(elapsed) %>%
  mutate(cum = cumsum(n), prob = cum / sum(n))


nod_fog_fit <- nls(prob ~ 1 - exp(-k * elapsed), nod_fog,
                   start = c(k = 0.1))

nod_by_day <- cleaned_events %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(EVENT_TYPE == "NOD", DATE >= today - 3 * 365, DATE < today) %>%
  mutate(
    elapsed = as.numeric(today - DATE)
  ) %>%
  mutate(weight = 1 / predict(nod_fog_fit, .)) %>%
  group_by(DATE) %>%
  summarize(observed = n(), predicted = sum(weight)) %>%
  mutate(
    elapsed = as.numeric(today - DATE)
  ) %>%
  mutate(
    confidence = predict(nod_fog_fit, .) ^ 2
  ) %>%
  select(-elapsed)

nod_trend <- lm(observed ~ DATE + cos(2*pi*as.numeric(DATE)/365) + sin(2*pi*as.numeric(DATE)/7) + cos(2*pi*as.numeric(DATE)/7),
                data = nod_by_day, subset = nod_by_day$confidence == 1)

nod_by_day <- nod_by_day %>%
  mutate(
    regressed = predict(nod_trend, .),
    combined = predicted * confidence + regressed * (1 - confidence),
    factor = combined / observed
  )

nod_prob_adjustment <- nod_by_day %>%
  mutate(outcome = "NOD", elapsed = today - DATE) %>%
  select(outcome, elapsed, factor)

next_step_probs <- next_step_probs %>%
  left_join(nod_prob_adjustment, by = c("outcome", "elapsed")) %>%
  mutate(value = ifelse(is.na(factor), value, value * factor)) %>%
  select(-factor)

prev_days <- seq(today - 6, today, by = "day")
sunday <- prev_days[weekdays(prev_days) == "Sunday"]

weeks <- seq(sunday - 3 * 52 * 7, sunday, by = 7)

backlogs <- working_events %>%
  select(outcome, next_step) %>%
  filter(complete.cases(.)) %>%
  unique() %>%
  merge(weeks, by = NULL) %>%
  select(outcome, next_step, obs_date = y)

backlogs.observed <- working_events %>%
  select(outcome, next_step, next_step_date, DATE) %>%
  count(start_week = cut(DATE, "week", start.on.monday = FALSE), end_week = cut(next_step_date, "week", start.on.monday = FALSE), outcome, next_step) %>%
  ungroup() %>%
  mutate(start_week = as.Date(start_week), end_week = as.Date(end_week)) %>%
  right_join(backlogs, by = c("outcome", "next_step")) %>%
  filter(start_week < obs_date, end_week >= obs_date) %>%
  group_by(outcome, next_step, obs_date) %>%
  summarise(n = sum(n)) %>%
  ungroup()

backlogs.added <- working_events %>%
  select(outcome, next_step, DATE) %>%
  count(obs_date = cut(DATE, "week", start.on.monday = FALSE), outcome, next_step) %>%
  ungroup() %>%
  mutate(obs_date = as.Date(obs_date)) %>%
  select(outcome, next_step, obs_date, added = n)

throughput <- working_events %>%
  filter(DATE >= weeks[1], DATE < sunday) %>%
  count(week = cut(DATE, "week", start.on.monday = FALSE), prev_outcome, step, outcome) %>%
  ungroup() %>%
  mutate(week = as.Date(week))

backlogs.removed <- throughput %>%
  select(obs_date = week, outcome = prev_outcome, next_step = step, n) %>%
  group_by(obs_date, outcome, next_step) %>%
  summarise(removed = sum(n)) %>%
  ungroup()

backlogs.predicted <- working_events %>%
  select(DATE, outcome, next_step, duration) %>%
  filter(is.na(next_step)) %>%
  select(-next_step) %>%
  inner_join(next_step_probs, by = c("outcome", "duration" = "elapsed")) %>%
  select(-duration) %>%
  arrange(DATE) %>%
  group_by(obs_date = cut(DATE, "week", start.on.monday = FALSE), outcome, next_step) %>%
  summarise(added = sum(value)) %>%
  ungroup() %>%
  mutate(obs_date = as.Date(obs_date)) %>%
  right_join(backlogs, by = c("outcome", "next_step", "obs_date")) %>%
  replace_na(list(added = 0)) %>%
  group_by(outcome, next_step) %>%
  mutate(n = cumsum(added) - added) %>%
  ungroup()

backlogs %<>%
  left_join(backlogs.observed, by = c("outcome", "next_step", "obs_date")) %>%
  left_join(backlogs.added, by = c("outcome", "next_step", "obs_date")) %>%
  left_join(backlogs.predicted, by = c("outcome", "next_step", "obs_date"), suffix = c(".observed", ".predicted")) %>%
  replace_na(list(n.observed = 0, n.predicted = 0, added.observed = 0, added.predicted = 0)) %>%
  mutate(n = round(n.observed + n.predicted), added = round(added.observed + added.predicted)) %>%
  left_join(backlogs.removed, by = c("outcome", "next_step", "obs_date")) %>%
  replace_na(list(removed = 0)) %>%
  mutate(net = added - removed)

backlogs_by_step <- backlogs %>%
  select(-outcome) %>%
  group_by(next_step, obs_date) %>%
  summarise_all(sum) %>%
  ungroup()

timing <- working_events %>%
  filter(!is.na(next_step)) %>%
  select(next_step, next_step_date, duration) %>%
  group_by(event_week = cut(next_step_date, "week", start.on.monday = FALSE), next_step) %>%
  summarise(mean = mean(duration), median = median(duration)) %>%
  ungroup() %>%
  mutate(event_week = as.Date(event_week)) %>%
  filter(event_week %in% weeks)




steps_to_plot <- c("SOC", "SSOC", "CERTIFICATION", "ACTIVATION", "CASE_REVIEW", "HEARING", "ASSIGNMENT", "DECISION", "OUTCODING")

for (step in steps_to_plot) {
  .plot <- ggplot(backlogs_by_step[backlogs_by_step$next_step == step,], aes(x = obs_date, y = n)) +
    ggtitle(paste0(step, " backlog over time (estimated)")) +
    geom_line() +
    expand_limits(y = 0)

  ggsave(paste0("appeals-process-charts/backlog-", step, ".png"), .plot)
}

for (step in steps_to_plot) {
  .plot <- ggplot(backlogs_by_step[backlogs_by_step$next_step == step & backlogs_by_step$obs_date != sunday ,], aes(x = obs_date, y = net)) +
    ggtitle(paste0(step, " throughput over time (estimated)")) +
    geom_area(aes(y = added), fill = "#BD2D28") +
    geom_area(aes(y = -removed), fill = "#0F8C79") +
    geom_line() +
    expand_limits(y = 0)

  ggsave(paste0("appeals-process-charts/throughput-", step, ".png"), .plot)
}

for (step in steps_to_plot) {
  .plot <- ggplot(timing[timing$next_step == step & timing$event_week != sunday ,], aes(x = event_week, y = median)) +
    ggtitle(paste0(step, " median time to event")) +
    geom_line() +
    expand_limits(y = 0)

  ggsave(paste0("appeals-process-charts/timing-", step, ".png"), .plot)
}

for (step in steps_to_plot) {
  .plot <- ggplot(timing[timing$next_step == step & timing$event_week != sunday ,], aes(x = event_week, y = mean)) +
    ggtitle(paste0(step, " mean time to event")) +
    geom_line() +
    expand_limits(y = 0)

  ggsave(paste0("appeals-process-charts/mean-timing-", step, ".png"), .plot)
}

ggplot(backlogs_by_step[backlogs_by_step$next_step %in% c("ACTIVATION", "CASE_REVIEW"),], aes(x = obs_date, y = n, fill = next_step)) +
  ggtitle("Combined ACTIVATION and CASE_REVIEW backlogs over time (estimated)") +
  geom_area()



bad_events <- gp_events[is.na(gp_events$step),]
bad_cases <- bad_events %>% select(BFCORLID, APPEAL_DATE) %>% unique()

bad_cases.compare <- gp_events %>% select(BFCORLID, APPEAL_DATE) %>% unique()

nrow(bad_cases) / nrow(bad_cases.compare)

ggplot(bad_cases, aes(APPEAL_DATE)) +
  geom_histogram(data = bad_cases.compare) +
  geom_histogram(fill = "red")

table(bad_events$EVENT_TYPE, bad_events$PREV_EVENT)
