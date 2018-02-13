install.packages(c("ROracle", "dplyr", "lubridate", "glue", "survival"))
source("R/vacolsConnect.R")
library(dplyr)
library(lubridate)
library(glue)
library(survival)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

# NOD -> SOC

offset = months(3)
window.l <- today() %m-% (offset + years(1))
window.r <- today() %m-% offset

cases <- query(glue("
select BFDNOD, BFDSOC, BFDC, BFDDEC, BFREGOFF
from BRIEFF
where BFDNOD is not null
  and BFDNOD < date '{window.r}'
  and (
    BFDSOC >= date '{window.l}'
    or (
      BFDSOC is null
      and (
        BFDDEC >= date '{window.l}'
        or BFMPRO = 'ADV'
      )
    )
  )
  and BFAC = '1'
")) %>%
  mutate(
    BFDNOD = as_date(BFDNOD),
    BFDSOC = as_date(BFDSOC),
    BFDDEC = as_date(BFDDEC),
    clsevent = factor(case_when(
      !is.na(BFDSOC) & BFDSOC < window.r ~ "soc",
      !is.na(BFDDEC) & BFDDEC < window.r ~ "other",
      TRUE                               ~ "censor"
    ), levels = c("censor", "soc", "other")),
    clsdate = case_when(
      clsevent == "censor" ~ window.r,
      clsevent == "soc"    ~ BFDSOC,
      clsevent == "other"  ~ BFDDEC
    ),
    age.enter = pmax(0, as.numeric(window.l - BFDNOD)),
    age.exit = as.numeric(clsdate - BFDNOD)
  ) %>%
  filter(age.enter < age.exit)

ros <- cases %>%
  transmute(ro = BFREGOFF) %>%
  count(ro) %>%
  filter(!is.na(ro), n >= 500)

ros$median_time <- sapply(ros$ro, function (x) {
  ro_cases <- filter(cases, BFREGOFF == x)
  msfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = ro_cases)

  mssumm <- summary(msfit)
  times <- data.frame(time = mssumm$time, pstate = mssumm$pstate[,1]) %>%
    mutate(prate = pstate / max(pstate))

  return(times$time[which.max(times$prate > 0.5)])
})

msfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = cases)

mssumm <- summary(msfit)
times <- data.frame(time = mssumm$time, pstate = mssumm$pstate[,1]) %>%
  mutate(prate = pstate / max(pstate))

ros <- ros %>%
  rbind(
    data.frame(
      ro = "*",
      n = nrow(cases),
      median_time = times$time[which.max(times$prate > 0.5)]
    )
  )

write.csv(ros, "output.csv", row.names = FALSE)

# Form 9 -> Certification or SSOC

offset = months(0)
window.l <- today() %m-% (offset + years(1))
window.r <- today() %m-% offset

cases <- query(glue("
select BFD19, BF41STAT, BFSSOC1, BFDDEC, BFREGOFF
from BRIEFF
where BFD19 is not null
  and BFD19 < date '{window.r}'
  and (
    BF41STAT >= date '{window.l}'
    or (
      BF41STAT is null
      and (
        BFDDEC >= date '{window.l}'
        or BFMPRO = 'ADV'
      )
    )
  )
  and BFAC = '1'
")) %>%
  mutate(
    BFD19 = as_date(BFD19),
    BF41STAT = as_date(BF41STAT),
    BFSSOC1 = as_date(BFSSOC1),
    BFDDEC = as_date(BFDDEC),
    clsevent = factor(case_when(
      !is.na(BF41STAT) & BF41STAT < window.r ~ "certification",
      !is.na(BFSSOC1) & BFSSOC1 < window.r   ~ "ssoc",
      !is.na(BFDDEC) & BFDDEC < window.r     ~ "other",
      TRUE                                   ~ "censor"
    ), levels = c("censor", "certification", "ssoc", "other")),
    clsdate = case_when(
      clsevent == "censor"        ~ window.r,
      clsevent == "certification" ~ BF41STAT,
      clsevent == "ssoc"          ~ BFSSOC1,
      clsevent == "other"         ~ BFDDEC
    ),
    age.enter = pmax(0, as.numeric(window.l - BFD19)),
    age.exit = as.numeric(clsdate - BFD19)
  ) %>%
  filter(age.enter < age.exit)

msfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = cases)

mssumm <- summary(msfit)
times <- data.frame(time = mssumm$time, pstate = mssumm$pstate[,1]) %>%
  mutate(prate = pstate / max(pstate))
times <- data.frame(time = mssumm$time, pstate = mssumm$pstate[,2]) %>%
  mutate(prate = pstate / max(pstate))

# Assignment -> Decision

offset = months(0)
window.l <- today() %m-% (offset + years(1))
window.r <- today() %m-% offset

cases <- query(glue("
select DEASSIGN, BFDC, BFDDEC
from BRIEFF
join (
  select DEFOLDER, min(DEASSIGN) DEASSIGN
  from DECASS
  group by DEFOLDER
) on BFKEY = DEFOLDER
where DEASSIGN < date '{window.r}'
  and (
    BFDDEC >= date '{window.l}'
    or BFMPRO = 'ACT'
  )
  and BFAC in ('1', '3')
")) %>%
  mutate(
    DEASSIGN = as_date(DEASSIGN),
    BFDDEC = as_date(BFDDEC),
    clsevent = factor(case_when(
      !is.na(BFDDEC) & BFDDEC < window.r & BFDC %in% c("1", "3", "4") ~ "decision",
      !is.na(BFDDEC) & BFDDEC < window.r ~ "other",
      TRUE                               ~ "censor"
    ), levels = c("censor", "decision", "other")),
    clsdate = case_when(
      clsevent == "censor"   ~ window.r,
      clsevent == "decision" ~ BFDDEC,
      clsevent == "other"    ~ BFDDEC
    ),
    age.enter = pmax(0, as.numeric(window.l - DEASSIGN)),
    age.exit = as.numeric(clsdate - DEASSIGN)
  ) %>%
  filter(age.enter < age.exit)

msfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = cases)

mssumm <- summary(msfit)
times <- data.frame(time = mssumm$time, pstate = mssumm$pstate[,1]) %>%
  mutate(prate = pstate / max(pstate))

# Remand -> SSOC

offset = months(0)
window.l <- today() %m-% (offset + years(1))
window.r <- today() %m-% offset

cases <- query(glue("
select BFDDEC, FIRSTSSOC, BFDLOOUT, BFMPRO
from BRIEFF
join (
  select BFKEY SSOCKEY,
    case when BFSSOC1 > BFDDEC then BFSSOC1
         when BFSSOC2 > BFDDEC then BFSSOC2
         when BFSSOC3 > BFDDEC then BFSSOC3
         when BFSSOC4 > BFDDEC then BFSSOC4
         when BFSSOC5 > BFDDEC then BFSSOC5
         end FIRSTSSOC
  from BRIEFF
) on BFKEY = SSOCKEY
where BFDDEC is not null
  and BFDDEC < date '{window.r}'
  and (
    FIRSTSSOC >= date '{window.l}'
    or (
      BFDLOOUT >= date '{window.l}'
      or BFMPRO = 'REM'
    )
  )
  and BFAC in ('1', '3')
  and BFDC in ('1', '3')
  and VACOLS.ISSUE_CNT_REMAND(BFKEY) > 0
")) %>%
  mutate(
    BFDDEC = as_date(BFDDEC),
    FIRSTSSOC = as_date(FIRSTSSOC),
    BFDLOOUT = as_date(BFDLOOUT),
    clsevent = factor(case_when(
      !is.na(FIRSTSSOC) & FIRSTSSOC < window.r ~ "ssoc",
      BFMPRO != 'REM' & !is.na(BFDLOOUT) & BFDLOOUT < window.r ~ "other",
      TRUE                               ~ "censor"
    ), levels = c("censor", "ssoc", "other")),
    clsdate = case_when(
      clsevent == "censor" ~ window.r,
      clsevent == "ssoc"   ~ FIRSTSSOC,
      clsevent == "other"  ~ BFDLOOUT
    ),
    age.enter = pmax(0, as.numeric(window.l - BFDDEC)),
    age.exit = as.numeric(clsdate - BFDDEC)
  ) %>%
  filter(age.enter < age.exit)

msfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = cases)

mssumm <- summary(msfit)
times <- data.frame(time = mssumm$time, pstate = mssumm$pstate[,1]) %>%
  mutate(prate = pstate / max(pstate))

# Remand SSOC -> SSOC2 / Return

offset = months(0)
window.l <- today() %m-% (offset + years(1))
window.r <- today() %m-% offset

cases <- query(glue("
select FIRSTSSOC, SECONDSSOC, REMRETURN, BFDLOOUT, BFMPRO
from BRIEFF
join (
  select BFKEY SSOCKEY,
    case when BFSSOC1 > BFDDEC then BFSSOC1
         when BFSSOC2 > BFDDEC then BFSSOC2
         when BFSSOC3 > BFDDEC then BFSSOC3
         when BFSSOC4 > BFDDEC then BFSSOC4
         when BFSSOC5 > BFDDEC then BFSSOC5
         end FIRSTSSOC,
    case when BFSSOC1 > BFDDEC then BFSSOC2
         when BFSSOC2 > BFDDEC then BFSSOC3
         when BFSSOC3 > BFDDEC then BFSSOC4
         when BFSSOC4 > BFDDEC then BFSSOC5
         end SECONDSSOC
  from BRIEFF
) on BFKEY = SSOCKEY
left join (
  select LOCKEY, max(LOCDOUT) REMRETURN
  from PRIORLOC
  where LOCSTTO = '96'
  group by LOCKEY
) on BFKEY = LOCKEY
where FIRSTSSOC is not null
  and FIRSTSSOC < date '{window.r}'
  and (
    FIRSTSSOC >= date '{window.l}'
    or (
      BFDLOOUT >= date '{window.l}'
      or BFMPRO = 'REM'
    )
  )
  and BFAC in ('1', '3')
  and BFDC in ('1', '3')
  and VACOLS.ISSUE_CNT_REMAND(BFKEY) > 0
")) %>%
  mutate(
    FIRSTSSOC = as_date(FIRSTSSOC),
    SECONDSSOC = as_date(SECONDSSOC),
    REMRETURN = as_date(REMRETURN),
    BFDLOOUT = as_date(BFDLOOUT),
    clsevent = factor(case_when(
      !is.na(SECONDSSOC) & SECONDSSOC < window.r ~ "ssoc",
      !is.na(REMRETURN) & REMRETURN < window.r ~ "return",
      BFMPRO != 'REM' & !is.na(BFDLOOUT) & BFDLOOUT < window.r ~ "other",
      TRUE                               ~ "censor"
    ), levels = c("censor", "ssoc", "return", "other")),
    clsdate = case_when(
      clsevent == "censor" ~ window.r,
      clsevent == "ssoc"   ~ SECONDSSOC,
      clsevent == "return" ~ REMRETURN,
      clsevent == "other"  ~ BFDLOOUT
    ),
    age.enter = pmax(0, as.numeric(window.l - FIRSTSSOC)),
    age.exit = as.numeric(clsdate - FIRSTSSOC)
  ) %>%
  filter(age.enter < age.exit)

msfit <- survfit(Surv(age.enter, age.exit, clsevent) ~ 1, data = cases)

mssumm <- summary(msfit)
times <- data.frame(time = mssumm$time, pstate = mssumm$pstate[,1]) %>%
  mutate(prate = pstate / max(pstate))
times <- data.frame(time = mssumm$time, pstate = mssumm$pstate[,2]) %>%
  mutate(prate = pstate / max(pstate))
