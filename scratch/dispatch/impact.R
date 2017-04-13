source("R/vacolsConnect.R")
source("R/caseflowConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")
library("ggplot2")
library("scales")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

cfcon <- caseflowConnect()
cfquery <- function (query) { return(dbGetQuery(cfcon, query)) }

decisions <- query("
  select
    BFKEY,
    BFCORLID,
    BFDDEC,
    TIOCTIME

  from BRIEFF

  inner join FOLDER on BFKEY = TICKNUM

  left join (
    select count(*) as ACT_ISSUES,
      ISSKEY

    from ISSUES

    where ISSDC = '3' or
      (ISSDC = '1' and not (ISSPROG = '02' and ISSCODE = '15' and ISSLEV1 = '04'))

    group by ISSKEY
  ) on BFKEY = ISSKEY

  where BFDDEC is not null
    and BFDC between '1' and '9'
    and ACT_ISSUES > 0
    and TIVBMS = 'Y'
    and TIOCTIME >= date '2016-04-01' and TIOCTIME < date '2017-04-01'
")

vets <- unique(decisions$BFCORLID)
write.table(vets, "sensitive_data/recent_ids_for_claims.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

claims <- read.csv("sensitive_data/claims.csv", stringsAsFactors = FALSE)

decisions %<>%
  group_by(BFCORLID, BFDDEC) %>%
  mutate(
    case_decision_date = as.Date(BFDDEC),
    outcoding_date = as.Date(TIOCTIME),
    multi_decision = n() > 1
  ) %>%
  ungroup()

merged_decisions_claims <- decisions %>%
  left_join(claims, by = c("BFCORLID")) %>%
  mutate(
    claim_decision_date = as.Date(claim_receive_date, "%m/%d/%Y"),
    journal_date = as.Date(journal_date, "%m/%d/%Y"),
    precise_match = case_decision_date == claim_decision_date,
    fuzzy_match = !precise_match & (abs(case_decision_date - claim_decision_date) <= 31 | abs(outcoding_date - claim_decision_date) <= 31)
  )

matches <- merged_decisions_claims %>%
  filter(precise_match | fuzzy_match) %>%
  select(BFCORLID, BFDDEC, precise_match, fuzzy_match) %>%
  group_by(BFCORLID, BFDDEC) %>%
  summarise(has_match = TRUE, has_precise_match = sum(precise_match) > 0)

merged_decisions_claims %<>%
  left_join(matches, by = c("BFCORLID", "BFDDEC"))

merged_decisions_claims$journal_date[merged_decisions_claims$journal_date < merged_decisions_claims$outcoding_date] <- NA

merged_decisions_claims %<>%
  group_by(BFCORLID, BFDDEC) %>%
  slice(which.min(ifelse(is.na(journal_date), Inf, journal_date))) %>%
  ungroup() %>%
  select(BFCORLID, BFDDEC, has_match, has_precise_match, benefit_claim_id, claim_decision_date, journal_date, journal_object_id, journal_station, journal_user_id, claim_type_code, claim_type_name, end_product_type_code, status_type_code)

decisions %<>%
  left_join(merged_decisions_claims, by = c("BFCORLID", "BFDDEC")) %>%
  mutate(
    time_to_ep = journal_date - outcoding_date,
    amc_ep = journal_station == 397
  )

clean_decisions <- decisions %>%
  filter(!multi_decision) %>%
  replace_na(list(has_match = FALSE, has_precise_match = FALSE))

by_month <- clean_decisions %>%
  mutate(
    month = as.Date(paste0(substr(TIOCTIME, 1, 7), "-01"))
  ) %>%
  group_by(month) %>%
  summarize(
    total = n(),
    matched = sum(has_match),
    precise_matched = sum(has_precise_match),
    within_a_week = sum(time_to_ep <= 7, na.rm = TRUE),
    median_time_to_ep = median(time_to_ep, na.rm = TRUE),
    x75_time_to_ep = quantile(time_to_ep, probs = 0.75, na.rm = TRUE),
    x95_time_to_ep = quantile(time_to_ep, probs = 0.95, na.rm = TRUE),
    adj_median_time_to_ep = median(c(time_to_ep, rep(Inf, times = sum(is.na(time_to_ep)))), na.rm = TRUE),
    adj_x75_time_to_ep = quantile(c(time_to_ep, rep(Inf, times = sum(is.na(time_to_ep)))), probs = 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    percent_matched = ifelse(row_number() <= n() - 3, matched / total, NA),
    percent_within_a_week = within_a_week / total
  )

caseflowCested <- cfquery("select vacols_id from tasks join appeals on appeal_id = appeals.id where type = 'EstablishClaim' and completion_status in ('0', '3')") %>%
  mutate(caseflow = TRUE)

usage_data <- clean_decisions %>%
  mutate(
    month = as.Date(paste0(substr(TIOCTIME, 1, 7), "-01"))
  ) %>%
  select(month, BFKEY) %>%
  left_join(caseflowCested, by = c("BFKEY" = "vacols_id")) %>%
  replace_na(list(caseflow = FALSE)) %>%
  group_by(month) %>%
  summarize(
    total = n(),
    caseflow = sum(caseflow)
  ) %>%
  ungroup() %>%
  mutate(rate = caseflow / total, last = n() == row_number()) %>%
  filter(month >= as.Date('2017-01-01'))

timing_data <- by_month %>%
  select(month, adj_median_time_to_ep, adj_x75_time_to_ep) %>%
  gather(key, value, adj_median_time_to_ep, adj_x75_time_to_ep) %>%
  mutate(key = ifelse(key == 'adj_median_time_to_ep', 'Median', '75th Percentile'))

matched_data <- by_month %>%
  select(month, percent_matched, percent_within_a_week) %>%
  gather(key, value, percent_matched, percent_within_a_week) %>%
  filter(complete.cases(.)) %>%
  mutate(key = ifelse(key == 'percent_matched', 'All Matching EPs', 'EPs Created within 7 Days')) %>%
  group_by(key) %>%
  mutate(last = n() == row_number()) %>%
  ungroup()

dva_theme <- theme_light() +
  theme(
    panel.border = element_blank(),
    text = element_text(family = "Source Sans Pro"),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color = "black"),
    legend.position = "top"
  )

timing_targets = c(4)

ggplot(timing_data, aes(x = month, y = value, color = key, label = paste(as.character(value), "days"))) +
  scale_y_continuous(limits = c(0, 30)) +
  scale_color_manual(values = c("#0F2D52", "#256FB2")) +
  geom_hline(yintercept = timing_targets, color = "#9F2F3F", linetype = "dashed", alpha = 0.5) +
  geom_text(data = data.frame(value = timing_targets, month = max(timing_data$month) + 30), color = "#9F2F3F", hjust = .3, vjust = -.33, size = 3, family = "Source Sans Pro", fontface = "bold") +
  stat_smooth(method = 'loess', span = 2, se = FALSE) +
  geom_point(pch = 4) +
  guides(color = guide_legend(title = '')) +
  dva_theme

ggsave("dispatch_timing.pdf", device = cairo_pdf, width = 5.5, height = 2.5, units = "in")

matched_targets = c(.85, .95)

ggplot(matched_data, aes(x = month, y = value, color = key, label = percent(value))) +
  scale_y_continuous(limits = c(0.5, 1), labels = percent) +
  scale_color_manual(values = c("#256FB2", "#0F2D52")) +
  geom_hline(yintercept = matched_targets, color = "#9F2F3F", linetype = "dashed", alpha = 0.5) +
  geom_text(data = data.frame(value = matched_targets, month = max(matched_data$month) + 30), color = "#9F2F3F", hjust = 0, vjust = -.33, size = 3, family = "Source Sans Pro", fontface = "bold") +
  geom_line(size = 1) +
  geom_point(data = subset(matched_data, last), size = 2) +
  geom_text(data = subset(matched_data, last), vjust = -.6, size = 3, family = "Source Sans Pro", fontface = "bold", show.legend = FALSE) +
  guides(color = guide_legend(title = '')) +
  dva_theme

ggsave("dispatch_matched.pdf", device = cairo_pdf, width = 5.5, height = 2.5, units = "in")

ggplot(usage_data, aes(x = month, y = rate, group = 1, label = percent(rate))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  geom_line(color = "#0F2D52", size = 1) +
  geom_point(data = subset(usage_data, last), color = "#0F2D52", size = 2) +
  geom_text(data = subset(usage_data, last), vjust = -.6, size = 3, family = "Source Sans Pro", fontface = "bold", show.legend = FALSE) +
  dva_theme

ggsave("dispatch_usage.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")

