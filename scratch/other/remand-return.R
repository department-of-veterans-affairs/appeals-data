## What percentage of remanded appeals return to BVA?

source("R/vacolsConnect.R")
library(dplyr)
library(survival)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

remands <- query("select BFCORLID, BFKEY, BFAC, BFDDEC from BRIEFF where BFDDEC >= date '2006-10-01' and BFDC between '1' and '9' and vacols.issue_cnt_remand(BFKEY) > 0 and BFAC in ('1', '3')")
returns <- query("select BFCORLID, BFKEY, BFDPDCN, TIDKTIME from BRIEFF join FOLDER on BFKEY = TICKNUM where BFDPDCN >= date '2006-10-01' and BFAC = '3' and (BFDC between '1' and '9' or BFDC is null)")

joined <- remands %>%
  left_join(returns, by = c("BFCORLID", "BFDDEC" = "BFDPDCN")) %>%
  mutate(
    BFDDEC = as.Date(BFDDEC),
    TIDKTIME = as.Date(TIDKTIME),
    age.enter = pmax(0, as.numeric((Sys.Date() - 365) - BFDDEC)),
    age.exit = as.numeric(ifelse(is.na(TIDKTIME), Sys.Date() - BFDDEC, TIDKTIME - BFDDEC)),
    status = !is.na(TIDKTIME)
  ) %>%
  filter(age.exit > age.enter, is.na(TIDKTIME) | TIDKTIME >= Sys.Date() - 365)

fit <- survfit(Surv(age.enter, age.exit, status) ~ 1, se.fit = T, data = joined)
fit.originals <- survfit(Surv(age.enter, age.exit, status) ~ 1, se.fit = T, data = subset(joined, BFAC == "1"))
fit.remands <- survfit(Surv(age.enter, age.exit, status) ~ 1, se.fit = T, data = subset(joined, BFAC == "3"))

plot(fit, xlab = "Days elapsed since decision", ylab = "Percent of remands outstanding")
summary(fit, times = 8 * 365 + 2)
plot(fit.originals, xlab = "Days elapsed since decision", ylab = "Percent of remands outstanding")
summary(fit.originals, times = 8 * 365 + 2)
plot(fit.remands, xlab = "Days elapsed since decision", ylab = "Percent of remands outstanding")
summary(fit.remands, times = 8 * 365 + 2)
