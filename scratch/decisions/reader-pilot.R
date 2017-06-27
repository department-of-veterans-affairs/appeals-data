source("R/vacolsConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")
library("ggplot2")
library("lme4")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

pilot <- read.csv("sensitive_data/reader_pilot.csv", stringsAsFactors = FALSE, strip.white = TRUE)

pilot$method <- factor(pilot$method, levels = c("VBMS", "Relativity", "Adobe", "Caseflow"))

m <- lm(hours ~ method, pilot)

vacols <- query(paste0("select BFCORLID, count(*) ISSUES, count(distinct(BFKEY)) CASES, sum(case when ISSDC = '1' then 1 else 0 end) ALLOWANCES, sum(case when ISSDC = '3' then 1 else 0 end) REMANDS, sum(case when ISSDC = '4' then 1 else 0 end) DENIALS from BRIEFF join ISSUES on BFKEY = ISSKEY where BFCORLID in ('", paste(pilot$appeal, collapse = "','"), "') and BFDDEC > date '2017-02-01' and ISSDC in ('1', '3', '4') and BFDC in ('1', '3', '4') group by BFCORLID"))

pilot_joined <- pilot %>%
  group_by(attorney) %>%
  filter(sum(method == "Caseflow") > 0) %>%
  ungroup() %>%
  inner_join(vacols, by = c("appeal" = "BFCORLID")) %>%
  group_by(appeal) %>%
  filter(n() == 1, CASES == 1) %>%
  ungroup()

m2 <- lm(hours ~ method + ISSUES + REMANDS, pilot_joined)
summary(m2)

m3 <- lmer(hours ~ method + ISSUES + REMANDS + (1 | attorney), pilot_joined)
summary(m3)
