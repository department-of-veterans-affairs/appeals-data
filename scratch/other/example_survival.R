install.packages(c("ROracle", "dplyr", "lubridate", "glue", "survival"))
source("R/vacolsConnect.R")
library(dplyr)
library(lubridate)
library(glue)
library(survival)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

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
