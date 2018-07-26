source("R/vacolsConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")
library("ggplot2")
library("knitr")
library("lubridate")
library("glue")
library("survival")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

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

adv_hearings <- query("
select BFD19,
  HEARING_DATE,
  TIDRECV,
  BFDDEC,
  BFDC
from BRIEFF
inner join FOLDER on BFKEY = TICKNUM
left join (
  select FOLDER_NR,
    min(HEARING_DATE) HEARING_DATE
  from HEARSCHED
  where HEARING_TYPE in ('C', 'T', 'V')
    and HEARING_DISP in ('H', 'C', 'N')
  group by FOLDER_NR
) on BFKEY = FOLDER_NR
where BFAC = '1'
  and BFHR in ('1', '2')
  and BFD19 is not null
  and (
    BFD19 >= date '2007-10-01'
    or HEARING_DATE >= date '2007-10-01'
    or TIDRECV >= date '2007-10-01'
    or BFDDEC >= date '2007-10-01'
    or BFMPRO = 'ADV'
  )
") %>%
  transmute(
    enter = as_date(BFD19),
    exit = as_date(pmin(HEARING_DATE, TIDRECV, BFDDEC, na.rm = TRUE)),
    BFDC
  ) %>%
  filter(is.na(exit) | enter <= exit)

rem_hearings <- query("
select BFDDEC, BFDLOOUT
from BRIEFF
join (
  select RMDKEY, count(case when RMDVAL = 'EA' then 1 end) HEARING_REMAND_CNT
  from RMDREA
  group by RMDKEY
) on BFKEY = RMDKEY
where BFDC in ('1', '3')
  and HEARING_REMAND_CNT > 0
  and (
    BFDDEC >= date '2007-10-01'
    or BFDLOOUT >= date '2007-10-01'
    or BFMPRO = 'REM'
  )
") %>%
  transmute(
    enter = as_date(BFDDEC),
    exit = as_date(BFDLOOUT)
  ) %>%
  filter(is.na(exit) | enter <= exit)

all_hearings <- rbind(adv_hearings, rem_hearings)

historical_backlog <- data.frame(date = seq(as_date("2007-10-01"), as_date("2018-05-16"), by = "week"))
historical_backlog$n <- sapply(historical_backlog$date, function(x) sum(all_hearings$enter < x & (is.na(all_hearings$exit) | all_hearings$exit >= x)))

ggplot(historical_backlog, aes(x = date, y = n)) +
  geom_line(color = "#0F2D52", size = .5) +
  dva_theme

ggsave("hearing_request_backlog.pdf", device = cairo_pdf, width = 4.5, height = 3, units = "in")

### Supplement for Lara ###
hearing_appeals_pending_decision <- query("select TIDRECV, BFDDEC from BRIEFF join FOLDER on BFKEY = TICKNUM where BFAC in ('1', '3', '7') and BFHR in ('1', '2') and TIDRECV < date '2017-10-01' and (BFMPRO = 'ACT' or BFDDEC >= date '2007-10-01')") %>%
  transmute(
    enter = as_date(TIDRECV),
    exit = as_date(BFDDEC)
  ) %>%
  filter(is.na(exit) | enter <= exit)

decision_backlog <- data.frame(date = seq(as_date("2007-10-01"), as_date("2017-10-01"), by = "week"))
decision_backlog$n <- sapply(decision_backlog$date, function(x) sum(hearing_appeals_pending_decision$enter < x & (is.na(hearing_appeals_pending_decision$exit) | hearing_appeals_pending_decision$exit >= x)))

ggplot(historical_backlog, aes(x = date, y = n)) +
  scale_y_continuous(limits = c(0, 90000)) +
  geom_line(color = "#0F2D52", size = .5) +
  geom_line(data = decision_backlog, color = "#256FB2") +
  dva_theme
##########################

hearings <- query("
select distinct FOLDER_NR, HEARING_PKSEQ, HEARING_DATE, HEARING_DISP
from HEARSCHED
where HEARING_TYPE in ('C', 'T', 'V')
  and HEARING_DISP in ('H', 'C', 'N')
  and HEARING_DATE >= date '2007-10-01'
  and HEARING_DATE < date '2017-10-01'
")

hearings.by_fy <- hearings %>%
  mutate(fy = (2008:2017)[findInterval(as.Date(HEARING_DATE), seq(as.Date('2007-10-01'), length=10, by='year'))]) %>%
  group_by(fy) %>%
  summarize(
    slots = n(),
    held = sum(HEARING_DISP == "H")
  )

hearing_surv_by_fy <- data.frame(
  fy = 2008:2017,
  start_date = seq(as.Date('2007-10-01'), length=10, by='year')
)

hearing_surv_by_fy$pct_hearing <- sapply(hearing_surv_by_fy$start_date, function (x) {
  cases <- query(glue("
    select BFD19, HEARING_DATE, TIDKTIME, BFDDEC, BFDC
    from BRIEFF
    join FOLDER on BFKEY = TICKNUM
    left join (
      select FOLDER_NR, min(HEARING_DATE) HEARING_DATE
      from HEARSCHED
      where HEARING_TYPE in ('C', 'T', 'V')
        and HEARING_DISP in ('H', 'C', 'N')
      group by FOLDER_NR
    ) on BFKEY = FOLDER_NR
    where BFAC = '1'
      and BFHR in ('1', '2')
      and (BFDC is null or BFDC <> 'M')
      and BFD19 < date '{end_date}'
      and (
        BFMPRO = 'ADV' or
        HEARING_DATE >= date '{start_date}' or
        TIDKTIME >= date '{start_date}' or
        (
          BFDC in ('A', 'E', 'F', 'G') and
          BFDDEC >= date '{start_date}'
        )
      )
  ", start_date = format(x, "%Y-%m-%d"), end_date = format(x + 365, "%Y-%m-%d"))) %>%
    mutate(
      BFD19 = as_date(BFD19),
      HEARING_DATE = as_date(HEARING_DATE),
      TIDKTIME = as_date(TIDKTIME),
      BFDDEC = as_date(BFDDEC),
      clsevent = factor(case_when(
        !is.na(HEARING_DATE) & HEARING_DATE < x + 365 ~ 'hearing',
        !is.na(TIDKTIME) & TIDKTIME < x + 365         ~ 'activation',
        is.na(BFDDEC) | BFDDEC >= x + 365             ~ 'censor',
        BFDC == 'A'                                   ~ 'field_grant',
        BFDC == 'E'                                   ~ 'death',
        BFDC %in% c('F', 'G')                         ~ 'withdrawn',
        TRUE                                          ~ 'censor'
      ), levels = c('censor', 'hearing', 'activation', 'field_grant', 'death', 'withdrawn')),
      clsdate = case_when(
        clsevent == 'hearing'    ~ HEARING_DATE,
        clsevent == 'activation' ~ TIDKTIME,
        clsevent == 'censor'     ~ x + 365,
        TRUE                     ~ BFDDEC
      ),
      age.enter = pmax(0, as.numeric(x - BFD19)),
      age.exit = as.numeric(clsdate - BFD19)
    ) %>%
    filter(age.enter < age.exit)

  msfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = cases)

  summary(msfit, times = max(cases$age.exit))$pstate[1]
})

requests <- query("
select BFD19
from BRIEFF
where BFAC = '1'
  and BFHR in ('1', '2')
  and BFD19 >= date '2007-10-01'
  and BFD19 < date '2017-10-01'
")

requests.by_fy <- requests %>%
  mutate(fy = (2008:2017)[findInterval(as.Date(BFD19), seq(as.Date('2007-10-01'), length=10, by='year'))]) %>%
  group_by(fy) %>%
  summarize(
    requests = n()
  ) %>%
  inner_join(hearing_surv_by_fy, by = "fy") %>%
  mutate(adj_requests = requests * pct_hearing)

combined <- hearings.by_fy %>%
  inner_join(requests.by_fy, by = "fy")

ggplot(combined, aes(x = fy)) +
  scale_x_continuous(breaks = seq(2009, 2017, 2)) +
  scale_y_continuous(limits = c(-30000, 30000), breaks = seq(-30000, 30000, 10000)) +
  geom_bar(aes(y = -adj_requests), fill = "#0F2D52", stat = "identity") +
  geom_bar(aes(y = slots), fill = "#256FB2", stat = "identity") +
  geom_line(aes(y = slots-adj_requests), color = "#9F2F3F", size = 1) +
  geom_point(aes(y = slots-adj_requests), color = "#9F2F3F", size = 2) +
  dva_theme

ggsave("hearings_net.pdf", device = cairo_pdf, width = 4.5, height = 3, units = "in")

docket <- read.csv("data/docket.csv", stringsAsFactors = FALSE) %>%
  mutate_all(as_date)

held_hearings <- query("
select distinct BFCORLID, HEARING_DATE, BFD19
from HEARSCHED
join BRIEFF on FOLDER_NR = BFKEY
where HEARING_TYPE in ('C', 'T', 'V')
  and HEARING_DISP = 'H'
  and HEARING_DATE >= date '2014-11-03'
  and BFD19 is not null
") %>%
  transmute(HEARING_DATE = as_date(HEARING_DATE), BFD19 = as_date(BFD19))

docket$med_docketed_hearing <- sapply(docket$date, function (x) quantile(as.numeric(subset(held_hearings, HEARING_DATE >= x & HEARING_DATE < (x + 7))$BFD19), probs = 0.75)) %>% as_date()

ggplot(docket, aes(x = date)) +
  scale_y_date(limits = as_date(c("2012-06-01", "2016-01-01"))) +
  geom_point(aes(y = docket), pch = 4, color = "#0F2D52") +
  geom_point(aes(y = med_docketed_hearing), pch = 4, color = "#256FB2") +
  geom_smooth(aes(y = docket), method = 'loess', span = 0.25, se = FALSE, color = "#0F2D52") +
  geom_smooth(aes(y = med_docketed_hearing), method = 'loess', span = 0.25, se = FALSE, color = "#256FB2") +
  dva_theme

ggsave("hearings_docket.pdf", device = cairo_pdf, width = 4.5, height = 3, units = "in")

ggplot(data = obs_rate_by_year.hearing, aes(x = fy, y = rate, color = key, linetype = "Observed")) +
  scale_x_continuous(name = 'Fiscal year', breaks = seq(2009, 2017, 2)) +
  scale_y_continuous(name = 'Rate', labels = percent) +
  scale_color_manual(values = c("#0F2D52", "#256FB2")) +
  scale_linetype_manual(values = c("Adjusted" = "dashed", "Observed" = "solid")) +
  geom_line() +
  geom_line(data = by_year, aes(y = hearing_rate, color = "All hearing", linetype = "Adjusted")) +
  geom_line(data = by_year, aes(y = baseline, color = "Non-hearing", linetype = "Adjusted")) +
  dva_theme +
  guides(color = FALSE, linetype = FALSE)

ggsave("hearings_effects.pdf", device = cairo_pdf, width = 4.5, height = 3, units = "in")

nod_cases <- query("
select BFDNOD, BFD19, BFDDEC
from BRIEFF
where BFAC = '1'
  and BFDNOD < date '2017-10-01'
  and (
    (BFD19 is null and BFMPRO = 'ADV')
    or BFD19 >= date '2016-10-01'
    or (
      BFDC in ('A', 'E', 'F', 'G')
      and BFDDEC >= date '2016-10-01'
    )
  )
") %>%
  mutate(
    clsevent = factor(case_when(
      !is.na(BFD19) & BFD19 < as_date("2017-10-01")   ~ "form9",
      !is.na(BFDDEC) & BFDDEC < as_date("2017-10-01") ~ "close",
      TRUE                                            ~ "censor"
    ), levels = c("censor", "form9", "close")),
    clsdate = case_when(
      clsevent == "form9" ~ as_date(BFD19),
      clsevent == "close" ~ as_date(BFDDEC),
      clsevent == "censor" ~ as_date("2017-10-01")
    ),
    age.enter = pmax(0, as.numeric(as_date("2016-10-01") - as_date(BFDNOD))),
    age.exit = as.numeric(clsdate - as_date(BFDNOD))
  ) %>%
  filter(age.enter < age.exit)

msfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = nod_cases)


future_backlog_by_month <- data.frame(month = seq(as_date("2017-12-01"), as_date("2027-11-01"), "month")) %>%
  mutate(
    elapsed = as.numeric(month - as_date("2017-11-20")),
    margin = elapsed - lag(elapsed),
    elapsed_since_feb14 = pmax(0, as.numeric(month - as_date("2019-02-14"))),
    form9_interp = 1 - summary(msfit, times = elapsed_since_feb14)$pstate[,1] / summary(msfit, times = max(elapsed_since_feb14))$pstate[,1],
    new_requests = cumsum(round(32615 / 365 * form9_interp * ifelse(is.na(margin), elapsed, margin))),
    resolutions = round((24000 * (elapsed - elapsed_since_feb14) + 16000 * elapsed_since_feb14) / 365)
  )

future_backlog_by_month$backlog = 82000

for (i in 2:nrow(future_backlog_by_month)) {
  prev <- future_backlog_by_month$backlog[i - 1]
  new_requests <- future_backlog_by_month$new_requests[i] - future_backlog_by_month$new_requests[i - 1]
  resolutions <- future_backlog_by_month$resolutions[i] - future_backlog_by_month$resolutions[i - 1]
  margin <- future_backlog_by_month$margin[i]
  closes <- round(prev * 3700 / 82500 / 365 * margin)

  future_backlog_by_month$backlog[i] <- max(0, prev + new_requests - resolutions - closes)
}

ggplot(future_backlog_by_month, aes(x = month, y = backlog)) +
  scale_y_continuous(breaks = seq(0, 80000, 20000)) +
  geom_line(color = "#0F2D52") +
  dva_theme

ggsave("hearings_legacy.pdf", device = cairo_pdf, width = 4.5, height = 3, units = "in")

moar_hearings <- query("
select distinct FOLDER_NR, HEARING_PKSEQ, HEARING_DATE, HEARING_DISP, HEARING_TYPE
from HEARSCHED
where HEARING_TYPE in ('C', 'T', 'V')
  and HEARING_DISP in ('H', 'C', 'N')
  and HEARING_DATE >= date '2016-10-01'
  and HEARING_DATE < date '2017-10-01'
")
