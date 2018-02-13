library(survival)

soc_issues <- query("
select
  BFDSOC,
  ISSDC,
  ISSDCLS,
  BF41STAT,
  BFSSOC1

from ISSUES

join BRIEFF on BFKEY = ISSKEY

where BFAC = '1'
  and BFDSOC < date '2017-10-01'
  and (
    (
      (
        ISSDCLS is not null
        or BF41STAT is not null
        or BFSSOC1 is not null
      )
      and least(nvl(ISSDCLS, date '2016-10-01'), nvl(BF41STAT, date '2016-10-01'), nvl(BFSSOC1, date '2016-10-01')) >= date '2016-10-01'
    )
    or (ISSDCLS is null and BF41STAT is null and BFSSOC1 is null and BFMPRO = 'ADV')
  )
") %>%
  mutate(
    clsevent = factor(case_when(
      !is.na(BFSSOC1) & (is.na(ISSDCLS) | BFSSOC1 < ISSDCLS) ~ 'no-grant',
      !is.na(BF41STAT) & (is.na(ISSDCLS) | BF41STAT < ISSDCLS) ~ 'certification',
      !is.na(ISSDCLS) & ISSDC == 'A' ~ 'grant',
      !is.na(ISSDCLS) ~ 'withdrawn',
      TRUE ~ 'censor'
    ), levels = c('censor', 'certification', 'grant', 'no-grant', 'withdrawn')),
    clsdate = case_when(
      clsevent == 'censor' ~ as_date('2017-10-01'),
      clsevent == 'certification' ~ as_date(BF41STAT),
      clsevent %in% c('grant', 'withdrawn') ~ as_date(ISSDCLS),
      clsevent == 'no-grant' ~ as_date(BFSSOC1)
    ),
    age.enter = pmax(0, as.numeric(as_date('2016-10-01') - as_date(BFDSOC))),
    age.exit = as.numeric(clsdate - as_date(BFDSOC))
  ) %>%
  filter(age.exit > age.enter)

msfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = soc_issues)
summary(msfit, times = max(soc_issues$age.exit))
plot(msfit)


docket.2014 <- query("
select
  BFD19,
  TIDRECV,
  BFDDEC,
  BFDC

from BRIEFF

join FOLDER on BFKEY = TICKNUM

where BFAC = '1'
  and BFSO <> 'A'
  and BFDC <> 'M'
  and BFD19 < date '2014-11-01'
  and (
    TIDRECV >= date '2014-11-01'
    or BFDDEC >= date '2014-11-01'
    or BFMPRO = 'ADV'
  )
")

docket.current <- query("
select BFD19
from BRIEFF
where BFAC = '1'
  and BFD19 < date '2017-10-01'
  and BFMPRO = 'ADV'
")

qplot(as_date(subset(docket.2014, BFD19 > as_date("2006-10-01"))$BFD19), geom = "density")
qplot(as_date(subset(docket.current, BFD19 > as_date("2009-10-01"))$BFD19), geom = "density")

docket.2014.ahead_of_docket <- docket.2014 %>% filter(BFD19 < as_date("2012-08-31"))
nrow(docket.2014.ahead_of_docket) / nrow(docket.2014)
sum(docket.2014.ahead_of_docket$BFDDEC < as_date("2016-02-14") & docket.2014.ahead_of_docket$BFDC %in% c('1', '3', '4'), na.rm = TRUE) / nrow(docket.2014.ahead_of_docket)

docket.2014.ahead_of_last_docket <- docket.2014 %>% filter(BFD19 < as_date("2013-07-31"))
nrow(docket.2014.ahead_of_last_docket) / nrow(docket.2014)
sum(docket.2014.ahead_of_last_docket$BFDDEC < as_date("2016-02-14") & docket.2014.ahead_of_last_docket$BFDC %in% c('1', '3', '4'), na.rm = TRUE) / nrow(docket.2014.ahead_of_last_docket)

docket.2014.other <- docket.2014 %>% filter(BFD19 >= as_date("2014-07-31"))
sum(docket.2014.other$BFDDEC < as_date("2016-02-14") & docket.2014.other$BFDC %in% c('1', '3', '4'), na.rm = TRUE) / nrow(docket.2014.other)

docket.2014.no_decision <- docket.2014.ahead_of_last_docket %>%
  filter(is.na(BFDDEC) | BFDDEC >= as_date("2016-02-14") | !(BFDC %in% c('1', '3', '4')))

sum((is.na(docket.2014.no_decision$BFDDEC) | docket.2014.no_decision$BFDDEC >= as_date("2016-02-14")) & (is.na(docket.2014.no_decision$TIDRECV) | docket.2014.no_decision$TIDRECV >= as_date("2016-02-14")))


docket.current.ahead_of_docket <- docket.current %>% filter(BFD19 < as_date("2014-02-28"))
nrow(docket.current.ahead_of_docket) / nrow(docket.current)


