source("R/vacolsConnect.R")

library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)
library(flexsurv)
library(ggfortify)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

form9 <- query("
select
  BFKEY,
  BFD19,
  BFDC,
  BFDDEC,
  TIDRECV

from BRIEFF

join FOLDER on BFKEY = TICKNUM

where BFD19 is not null
  and BFAC = '1'
  and (
    BFMPRO = 'ADV'
    or (TIDRECV >= date '2016-08-30' and TIDRECV < date '2017-08-30')
    or (BFDC in ('A', 'E', 'F', 'G') and BFDDEC >= date '2016-08-30' and BFDDEC < date '2017-08-30')
  )
") %>%
  mutate(
    BFD19 = as_date(BFD19),
    BFDDEC = as_date(BFDDEC),
    TIDRECV = as_date(TIDRECV),
    clsevent = factor(ifelse(!is.na(TIDRECV), 'activation', ifelse(BFDC == 'A', 'field', ifelse(BFDC %in% c('E', 'F', 'G'), 'withdrawn', 'censor'))), levels = c('censor', 'field', 'withdrawn', 'activation')),
    clsdate = as_date(ifelse(clsevent == 'activation', TIDRECV, ifelse(clsevent == 'field', BFDDEC, NA))),
    age.enter = pmax(0, as.numeric(as_date('2016-08-30') - BFD19)),
    age.exit = as.numeric(ifelse(is.na(clsdate), as_date('2017-08-30') - BFD19, clsdate - BFD19))
  ) %>%
  replace_na(list(clsevent = 'censor')) %>%
  filter(age.exit > age.enter)

msfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = form9)
plot(msfit)

res <- summary(msfit, times = seq(1, max(form9$age.exit), 1))
tbl <- data.frame(time = res$time, pstate = res$pstate)
colnames(tbl)[2:5] <- res$states
colnames(tbl)[5] <- "remaining"
marg <- tbl %>% mutate(
    marg_field = (max(field) - field) / remaining,
    marg_withdrawn = (max(withdrawn) - withdrawn) / remaining,
    marg_activation = (max(activation) - activation) / remaining
  ) %>%
  filter(remaining > 0.001)

censored <- form9 %>%
  filter(clsevent == "censor") %>%
  inner_join(marg, by = c("age.exit" = "time"))

sum(censored$marg_activation) / nrow(censored) # What percentage of current ADV cases do we expect to ever be activated?

activationreg <- flexsurvreg(Surv(age.enter, age.exit, clsevent == "activation") ~ 1, data = subset(form9, age.exit < 1750), dist = "weibull")

shape <- activationreg$res[1, 1]
scale <- activationreg$res[2, 1]

diff <- tbl %>%
  mutate(
    future = lead(activation, 365*2),
    diff_surv = (future - activation) / remaining,
    weibull = pweibull(time, shape = shape, scale = scale),
    future_weibull = lead(weibull, 365*2),
    diff_weibull = (future_weibull - weibull) / (1 - weibull)
  )

censored <- form9 %>%
  filter(clsevent == "censor") %>%
  inner_join(diff, by = c("age.exit" = "time"))

nrow(censored)
sum(censored$diff_surv, na.rm = TRUE) # How many ADV cases do we expect to be activated in the next year?
sum(censored$diff_weibull, na.rm = TRUE) # How does our distribution compare?

ggplot(diff, aes(x = time)) +
  geom_line(aes(y = diff_surv), color = "red") +
  geom_line(aes(y = diff_weibull), color = "blue")

censored.cutoff <- censored %>%
  filter(age.exit > 365*2)

nrow(censored.cutoff)
sum(censored.cutoff$diff_surv, na.rm = TRUE)
sum(censored.cutoff$diff_weibull, na.rm = TRUE)

remands <- query("
select
  BFDDEC
from BRIEFF
where BFMPRO = 'REM'
  and BFAC in ('1', '3')
") %>%
  transmute(
    clsevent = "censor",
    age.enter = pmax(0, as.numeric(as_date('2016-08-30') - as_date(BFDDEC))),
    age.exit = as.numeric(as_date('2017-08-30') - as_date(BFDDEC))
  )

returns <- query("
select
  BFCORLID,
  BFDPDCN,
  BFDC,
  BFDDEC,
  TIDKTIME
from BRIEFF
join FOLDER on BFKEY = TICKNUM
where BFAC = '3'
  and (
    (
      BFDC in ('B', 'W')
      and BFDDEC >= date '2016-08-30'
      and BFDDEC < date '2017-08-30'
    ) or (
      (BFDC between '1' and '9' or BFDC is null)
      and TIDKTIME >= date '2016-08-30'
      and TIDKTIME < date '2017-08-30'
    )
  )
") %>%
  transmute(
    clsevent = case_when(
      BFDC == "B" ~ "field",
      BFDC == "W" ~ "withdrawn",
      TRUE        ~ "return"
    ),
    age.enter = pmax(0, as.numeric(as_date('2016-08-30') - as_date(BFDPDCN))),
    age.exit = as.numeric(as_date(ifelse(clsevent == "return", as_date(TIDKTIME), as_date(BFDDEC))) - as_date(BFDPDCN))
  )

all_remands <- rbind(remands, returns) %>%
  mutate(clsevent = factor(clsevent, levels = c('censor', 'field', 'withdrawn', 'return'))) %>%
  filter(age.exit > age.enter)

remandsfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = all_remands)
plot(remandsfit)
summary(remandsfit, times = 7000)

remandreg <- flexsurvreg(Surv(age.enter, age.exit, clsevent != "censor") ~ 1, data = subset(all_remands, age.exit < 2000), dist = "weibull")
remandshape <- remandreg$res[1, 1]
remandscale <- remandreg$res[2, 1]
remandmean <- remandscale * gamma(1 + 1 / remandshape)

orig_activations <- query("
select
  BFDC,
  BFDDEC,
  TIDKTIME
from BRIEFF
join FOLDER on BFKEY = TICKNUM
where BFAC = '1'
  and (
    (
      BFDC between '1' and '9'
      and BFDDEC >= date '2016-09-06'
      and BFDDEC < date '2017-09-06'
    ) or (
      BFDC is null and BFMPRO = 'ACT' and TIDKTIME is not null
    )
  )
") %>%
  transmute(
    clsevent = factor(case_when(
      is.na(BFDC)          ~ "censor",
      BFDC %in% c("6", "8") ~ "withdrawn",
      TRUE                 ~ "decision"
    ), levels = c("censor", "withdrawn", "decision")),
    age.enter = pmax(0, as.numeric(as_date('2016-09-06') - as_date(TIDKTIME))),
    age.exit = as.numeric(as_date(ifelse(clsevent == "censor", as_date("2017-09-06"), as_date(BFDDEC))) - as_date(TIDKTIME))
  ) %>%
  filter(age.exit > age.enter)

orig_activationsfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = orig_activations)
plot(orig_activationsfit)

summary(orig_activationsfit, times = 2000)

decisions <- query("
select count(*) N, sum(case when vacols.issue_cnt_remand(BFKEY) = 0 then 1 else 0 end) FINAL_N, sum(case when vacols.issue_cnt_remand(BFKEY) > 0 then 1 else 0 end) REMAND_N
from BRIEFF
where BFAC in ('1', '3')
  and BFDC in ('1', '3', '4')
  and vacols.aod_cnt(BFKEY) = 0
  and BFDDEC >= date '2016-08-30'
  and BFDDEC < date '2017-08-30'
")

final_decisions <- decisions$FINAL_N / 365
remand_decisions <- decisions$REMAND_N / 365

remand_steady_state <- remandmean * remand_decisions

other_resolutions <- query("
select count(*) N
from BRIEFF
where (
    (BFAC in ('1', '3') and BFDC in ('B', 'W'))
    or (BFAC = '3' and BFDC in ('6', '8'))
  )
  and vacols.aod_cnt(BFKEY) = 0
  and BFDDEC >= date '2016-08-30'
  and BFDDEC < date '2017-08-30'
")$N / 365

docket_margin <- query("
select count(*) N
from BRIEFF
where BFMPRO = 'ACT'
  and BFAC in ('1', '3')
  and vacols.aod_cnt(BFKEY) = 0
  and BFD19 < date '2013-12-01'
")$N

ahead_on_docket <- query("
select
  BFD19,
  BFMPRO,
  BFAC,
  (case when BFCURLOC in ('16', '19', '20', '81', '82', '83', '84', '85') or length(BFCURLOC) > 2 then 1 else 0 end) READY
from BRIEFF
where BFMPRO in ('ADV', 'ACT', 'REM')
  and BFAC in ('1', '3')
  and vacols.aod_cnt(BFKEY) = 0
  and BFD19 < date '2014-06-25'
") %>%
  mutate(BFD19 = as_date(BFD19))

naive_t <- (nrow(ahead_on_docket) - sum(ahead_on_docket$BFAC == "1") * 0.0536 - remand_steady_state - docket_margin) / (final_decisions + other_resolutions)

advance <- ahead_on_docket %>%
  filter(BFMPRO == 'ADV') %>%
  mutate(
    time = as.numeric(as_date('2017-09-06') - BFD19),
    weibull = pweibull(time, shape = shape, scale = scale),
    future_weibull = pweibull(time + naive_t, shape = shape, scale = scale),
    diff_weibull = (future_weibull - weibull) / (1 - weibull)
  )

exp_adv <- nrow(advance) - sum(advance$diff_weibull, na.rm = TRUE)

rem_interp <- min(naive_t / remandmean, 1)

exp_rem <- remand_steady_state * rem_interp + sum(ahead_on_docket$BFMPRO == 'REM') * (1 - rem_interp)

final_t <- max(0, (nrow(ahead_on_docket) - exp_adv - (sum(ahead_on_docket$BFAC == "1") - exp_adv) * 0.0536 - exp_rem - docket_margin) / (final_decisions + other_resolutions))

predicted_date <- as_date('2017-09-06') + final_t

behind_on_docket <- query("
select
  count(*) N
from BRIEFF
where BFMPRO in ('ADV', 'ACT', 'REM')
  and BFAC in ('1', '3')
  and vacols.aod_cnt(BFKEY) = 0
  and BFD19 >= date '2014-06-25'
")$N





by_month <- data.frame(month = seq(as_date("2013-09-01"), as_date("2017-09-01"), "1 month"))

docket_date_cases <- query("
select LOCDOUT, LOCDIN, BFD19
from PRIORLOC
join BRIEFF on LOCKEY = BFKEY
where LOCSTTO = '81'
  and (LOCDIN is null or LOCDIN >= date '2013-09-01')
")

by_month$docket_date <- as_date(apply(by_month, 1, function(x) {
  .docket <- docket_date_cases %>%
    filter(
      LOCDOUT < x["month"],
      is.na(LOCDIN) | LOCDIN >= x["month"]
    ) %>%
    arrange(BFD19)
  return(as_date(.docket$BFD19[3000]))
}))

docket_margin_cases <- query("
select BFD19, TIDRECV, BFDDEC
from BRIEFF
join FOLDER on BFKEY = TICKNUM
where BFAC in ('1', '3')
  and vacols.aod_cnt(BFKEY) = 0
  and (BFDDEC >= date '2013-09-01' or BFDDEC is null)
")

by_month$docket_margin <- apply(by_month, 1, function(x) {
  .docket_margin <- docket_margin_cases %>%
    filter(
      BFD19 < x["docket_date"],
      TIDRECV < x["month"],
      is.na(BFDDEC) | BFDDEC >= x["month"]
    )
  return(nrow(.docket_margin))
})

decision_cases <- query("
select BFDDEC,
  (case when vacols.issue_cnt_remand(BFKEY) = 0 then 1 else 0 end) FINAL
from BRIEFF
where BFAC in ('1', '3')
  and BFDC in ('1', '3', '4')
  and vacols.aod_cnt(BFKEY) = 0
  and BFDDEC >= date '2012-09-01'
")

by_month$final_decision_rate <- apply(by_month, 1, function(x) {
  .yearago <- as_date(x["month"]) - 365
  .final_decisions <- decision_cases %>%
    filter(
      FINAL,
      BFDDEC < x["month"],
      BFDDEC >= .yearago
    ) %>%
    nrow() / 365

  return(.final_decisions)
})

by_month$remand_rate <- apply(by_month, 1, function(x) {
  .yearago <- as_date(x["month"]) - 365
  .remands <- decision_cases %>%
    filter(
      !FINAL,
      BFDDEC < x["month"],
      BFDDEC >= .yearago
    ) %>%
    nrow() / 365

  return(.remands)
})

other_resolutions_cases <- query("
select BFDDEC
from BRIEFF
where (
    (BFAC in ('1', '3') and BFDC in ('B', 'W'))
    or (BFAC = '3' and BFDC in ('6', '8'))
  )
  and vacols.aod_cnt(BFKEY) = 0
  and BFDDEC >= date '2012-09-01'
")

by_month$other_resolution_rate <- apply(by_month, 1, function(x) {
  .yearago <- as_date(x["month"]) - 365
  .other_resolutions <- other_resolutions_cases %>%
    filter(
      BFDDEC < x["month"],
      BFDDEC >= .yearago
    ) %>%
    nrow() / 365

  return(.other_resolutions)
})

form9_cases <- query("
select
  BFD19,
  BFDC,
  BFDDEC,
  TIDRECV

from BRIEFF

join FOLDER on BFKEY = TICKNUM

where BFD19 is not null
  and BFAC = '1'
  and (
    BFMPRO = 'ADV'
    or (TIDRECV >= date '2012-09-01')
    or (BFDC in ('A', 'E', 'F', 'G') and BFDDEC >= date '2012-09-01')
  )
")

by_month$adv_shape <- NA
by_month$adv_scale <- NA
by_month$adv_withdrawn <- NA

for(i in 1:nrow(by_month)) {
  .month <- by_month$month[i]

  .data <- form9_cases %>%
    filter(
      BFD19 < .month,
      (is.na(TIDRECV) & is.na(BFDC)) |
      (!is.na(TIDRECV) & TIDRECV >= .month - 365) |
      (!is.na(BFDC) & BFDC %in% c('A', 'E', 'F', 'G') & BFDDEC >= .month - 365)
    ) %>%
    mutate(
      BFD19 = as_date(BFD19),
      BFDDEC = as_date(BFDDEC),
      TIDRECV = as_date(TIDRECV),
      clsevent = factor(ifelse(TIDRECV < .month & !is.na(TIDRECV), 'activation', ifelse(BFDDEC < .month & BFDC == 'A', 'field', ifelse(BFDDEC < .month & BFDC %in% c('E', 'F', 'G'), 'withdrawn', 'censor'))), levels = c('censor', 'field', 'withdrawn', 'activation')),
      clsdate = as_date(ifelse(clsevent == 'activation', TIDRECV, ifelse(clsevent == 'field', BFDDEC, NA))),
      age.enter = pmax(0, as.numeric(.month - 365 - BFD19)),
      age.exit = as.numeric(ifelse(is.na(clsdate), .month - BFD19, clsdate - BFD19))
    ) %>%
    replace_na(list(clsevent = 'censor')) %>%
    filter(age.exit > age.enter)

  .fit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = .data)
  .advwithdraw <- summary(.fit, times = max(.data$age.exit))$pstate[1,2]
  .advreg <- flexsurvreg(Surv(age.enter, age.exit, clsevent == "activation") ~ 1, data = subset(.data, age.exit < 1500), dist = "weibull")

  by_month$adv_shape[i] <- .advreg$res[1, 1]
  by_month$adv_scale[i] <- .advreg$res[2, 1]
  by_month$adv_withdrawn[i] <- .advwithdraw
}

remand_cases <- query("
select
  BFDDEC,
  BFCURLOC,
  BFDLOOUT
from BRIEFF
where BFDC in ('1', '3')
  and VACOLS.ISSUE_CNT_REMAND(BFKEY) > 0
  and BFAC in ('1', '3')
  and (BFCURLOC <> '99' or BFDLOOUT >= date '2012-09-01')
")

return_cases <- query("
select
  BFCORLID,
  BFDPDCN,
  BFDC,
  BFDDEC,
  TIDRECV
from BRIEFF
join FOLDER on BFKEY = TICKNUM
where BFAC = '3'
  and (
    (
      BFDC in ('B', 'W')
      and BFDDEC >= date '2012-09-01'
    ) or TIDRECV >= date '2012-09-01'
  )
")

by_month$mean_remand_time <- apply(by_month, 1, function(x) {
  .month <- as_date(x["month"])
  .yearago <- .month - 365

  .remands <- remand_cases %>%
    filter(
      BFDDEC < .month,
      BFCURLOC != '99' | BFDLOOUT >= .month
    ) %>%
    transmute(
      clsevent = "censor",
      age.enter = pmax(0, as.numeric(.yearago - as_date(BFDDEC))),
      age.exit = as.numeric(.month - as_date(BFDDEC))
    )

  .returns <- return_cases %>%
    filter(
      (
        !is.na(BFDC) & (
          BFDC %in% c('B', 'W')
          & BFDDEC >= .yearago
          & BFDDEC < .month
        )
      )
      | (
        !is.na(TIDRECV) &(
          TIDRECV >= .yearago
          & TIDRECV < .month
        )
      )
    ) %>%
    transmute(
      clsevent = case_when(
        BFDC == "B" ~ "field",
        BFDC == "W" ~ "withdrawn",
        TRUE        ~ "return"
      ),
      age.enter = pmax(0, as.numeric(.yearago - as_date(BFDPDCN))),
      age.exit = as.numeric(as_date(ifelse(clsevent == "return", as_date(TIDRECV), as_date(BFDDEC))) - as_date(BFDPDCN))
    )

  .all_remands <- rbind(.remands, .returns) %>%
    mutate(clsevent = factor(clsevent, levels = c('censor', 'field', 'withdrawn', 'return'))) %>%
    filter(age.exit > age.enter)

  .remandreg <- flexsurvreg(Surv(age.enter, age.exit, clsevent != "censor") ~ 1, data = subset(.all_remands, age.exit < 1200), dist = "weibull")
  .remandshape <- .remandreg$res[1, 1]
  .remandscale <- .remandreg$res[2, 1]
  .remandmean <- .remandscale * gamma(1 + 1 / .remandshape)

  return(.remandmean)
})

orig_activation_cases <- query("
select
  BFDC,
  BFDDEC,
  TIDRECV
from BRIEFF
join FOLDER on BFKEY = TICKNUM
where BFAC = '1'
  and TIDRECV < date '2013-09-01'
  and (
    (
      BFDC between '1' and '9'
      and BFDDEC >= date '2012-09-01'
    ) or BFMPRO = 'ACT'
  )
")

by_month$orig_bva_withdrawn_rate <- apply(by_month, 1, function(x) {
  .month <- as_date(x["month"])
  .yearago <- .month - 365

  .orig_activations <- orig_activation_cases %>%
    filter(
      TIDRECV < .month,
      (is.na(BFDDEC) | BFDDEC >= .yearago)
    ) %>%
    transmute(
      clsevent = factor(case_when(
        is.na(BFDC)          ~ "censor",
        BFDC %in% c("6", "8") ~ "withdrawn",
        TRUE                 ~ "decision"
      ), levels = c("censor", "withdrawn", "decision")),
      age.enter = pmax(0, as.numeric(.yearago - as_date(TIDRECV))),
      age.exit = as.numeric(as_date(ifelse(clsevent == "censor", .month, as_date(BFDDEC))) - as_date(TIDRECV))
    ) %>%
    filter(age.exit > age.enter)


  .orig_activationsfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = .orig_activations)
  .withdrawn_rate <- summary(.orig_activationsfit, times = 2000)$pstate[1,1]

  return(.withdrawn_rate)
})


docket_cases <- query("
select
  BFAC,
  BFDC,
  BFD19,
  TIDRECV,
  BFDDEC,
  BFCURLOC,
  BFDLOOUT
from BRIEFF
join FOLDER on BFKEY = TICKNUM
where BFAC in ('1', '3')
  and vacols.aod_cnt(BFKEY) = 0
  and BFD19 is not null
  and (BFDDEC is null or BFDDEC >= date '2010-09-01')
") %>%
  mutate(BFD19 = as_date(BFD19), BFDDEC = as_date(BFDDEC), BFDLOOUT = as_date(BFDLOOUT))

predictions <- data.frame()

for(i in 15:nrow(by_month)) {
  .month <- by_month$month[i]
  .docket_date <- by_month$docket_date[i]
  .docket_margin <- by_month$docket_margin[i]
  .final_decision_rate <- by_month$final_decision_rate[i]
  .remand_rate <- by_month$remand_rate[i]
  .other_resolution_rate <- by_month$other_resolution_rate[i]
  .adv_shape <- by_month$adv_shape[i]
  .adv_scale <- by_month$adv_scale[i]
  .mean_remand_time <- by_month$mean_remand_time[i]
  .orig_bva_withdrawn_rate <- by_month$orig_bva_withdrawn_rate[i]

  .remand_steady_state <- .remand_rate * .mean_remand_time

  .predictions <- data.frame(
      form9_date = seq(as_date("2010-09-01"), as_date("2017-09-01"), "1 month"),
      prediction_date = .month,
      ahead_on_docket_cnt = NA,
      orig_ahead_on_docket_cnt = NA,
      adv_ahead_on_docket_cnt = NA,
      rem_ahead_on_docket_cnt = NA,
      initial_t = NA,
      exp_adv = NA,
      rem_interp = NA,
      exp_rem = NA,
      final_t = NA,
      predicted_date = as_date(NA)
    ) %>%
    filter(form9_date <= prediction_date)

  for(j in 1:nrow(.predictions)) {
    .form9_date <- .predictions$form9_date[j]

    .ahead_on_docket <- docket_cases %>%
      filter(BFD19 < .form9_date, BFCURLOC != '99' | BFDLOOUT >= .month)

    .ahead_on_docket_cnt <- nrow(.ahead_on_docket)

    .orig_ahead_on_docket <- .ahead_on_docket %>%
      filter(BFAC == '1', is.na(BFDDEC) | BFDDEC >= .month)

    .orig_ahead_on_docket_cnt <- .orig_ahead_on_docket %>% nrow()

    .adv_ahead_on_docket <- .orig_ahead_on_docket %>%
      filter(
        (is.na(TIDRECV) & is.na(BFDDEC)) |
        (!is.na(TIDRECV) & TIDRECV >= .month) |
        (!is.na(BFDC) & BFDC %in% c('A', 'E', 'F', 'G') & BFDDEC >= .month)
      )

    .adv_ahead_on_docket_cnt <- nrow(.adv_ahead_on_docket)

    .rem_ahead_on_docket_cnt <- .ahead_on_docket %>%
      filter(BFDDEC < .month) %>%
      nrow()

    .initial_t <- (.ahead_on_docket_cnt - .adv_ahead_on_docket_cnt - .remand_steady_state - .docket_margin) / (.final_decision_rate + .other_resolution_rate)

    .t <- .initial_t

    repeat {
      .exp_adv_act <- .adv_ahead_on_docket %>%
        mutate(
          time = as.numeric(.month - BFD19),
          weibull = pweibull(time, shape = .adv_shape, scale = .adv_scale),
          future_weibull = pweibull(time + .t, shape = .adv_shape, scale = .adv_scale),
          diff_weibull = (future_weibull - weibull) / (1 - weibull)
        ) %>%
        pull(diff_weibull) %>%
        sum(na.rm = TRUE)

      .exp_adv <- .adv_ahead_on_docket_cnt - .exp_adv_act

      .rem_interp <- max(min(.t / .mean_remand_time, 1), 0)

      .exp_rem <- .remand_steady_state * .rem_interp + .rem_ahead_on_docket_cnt * (1 - .rem_interp)

      .t1 <- max(0, (.ahead_on_docket_cnt - .exp_adv - (.adv_ahead_on_docket_cnt - .exp_adv) * 0.19 - .orig_ahead_on_docket_cnt * 0.04 - .exp_rem - .docket_margin) / (.final_decision_rate + .other_resolution_rate))

      if (abs(.t - .t1) < 1) { break }

      .t <- .t1
    }

    .final_t <- .t1

    .predicted_date <- .month + .final_t

    .predictions[j,]$ahead_on_docket_cnt      <- .ahead_on_docket_cnt
    .predictions[j,]$orig_ahead_on_docket_cnt <- .orig_ahead_on_docket_cnt
    .predictions[j,]$adv_ahead_on_docket_cnt  <- .adv_ahead_on_docket_cnt
    .predictions[j,]$rem_ahead_on_docket_cnt  <- .rem_ahead_on_docket_cnt
    .predictions[j,]$initial_t                <- .initial_t
    .predictions[j,]$exp_adv                  <- .exp_adv
    .predictions[j,]$rem_interp               <- .rem_interp
    .predictions[j,]$exp_rem                  <- .exp_rem
    .predictions[j,]$final_t                  <- .final_t
    .predictions[j,]$predicted_date           <- .predicted_date
  }

  predictions <- rbind(predictions, .predictions)
}

ggplot(predictions, aes(x = prediction_date, y = predicted_date, color = factor(form9_date))) +
  geom_line() +
  scale_color_discrete(guide = "none")

