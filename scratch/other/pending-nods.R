# How long have NODs been waiting, by RO

source("R/vacolsConnect.R")

library("dplyr")
library("magrittr")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

test <- query("select BFKEY, BFDNOD, BFDRODEC, BFREGOFF from BRIEFF where BFDNOD is not null and BFDRODEC is not null and BFDSOC is null and BFMPRO = 'ADV' and BFAC = '1'")

test.1 <- test %>%
  mutate(
    BFDNOD = as.Date(BFDNOD),
    BFDRODEC = as.Date(BFDRODEC),
    timely = BFDNOD <= BFDRODEC + 365,
    timely_plus3 = BFDNOD <= BFDRODEC + 365 + 3,
    timely_plus7 = BFDNOD <= BFDRODEC + 365 + 7
  )

nod_pending <- query("select BFDNOD, BFREGOFF from BRIEFF where BFDNOD is not null and BFDSOC is null and BFAC = '1' and BFMPRO = 'ADV'") %>%
  mutate(
    BFDNOD = as.Date(BFDNOD),
    days_pending = as.numeric(Sys.Date() - BFDNOD)
  ) %>%
  group_by(BFREGOFF) %>%
  summarize(
    total = n(),
    yrs_pending.50 = quantile(days_pending, probs = 0.5) / 365,
    yrs_pending.75 = quantile(days_pending, probs = 0.75) / 365,
    yrs_pending.95 = quantile(days_pending, probs = 0.95) / 365,
    yrs_pending.max = max(days_pending) / 365
  )

socs_per_yr <- query("select count(*) FY17_SOCS, BFREGOFF from BRIEFF where BFDSOC >= date '2016-10-01' and BFDSOC < date '2017-10-01' and BFAC = '1' group by BFREGOFF")

nod_pending %<>%
  left_join(socs_per_yr, by = c("BFREGOFF")) %>%
  mutate(backlog_ratio = total / FY17_SOCS)

ros <- read.csv("data/ro.csv", stringsAsFactors = FALSE)

nod_pending %<>%
  left_join(ros, by = c("BFREGOFF"))

nod_pending %>%
  transmute(
    RO = BFREGOFF,
    Name,
    Pending_NODs = total,
    FY17_SOCs = FY17_SOCS,
    Backlog_Ratio = backlog_ratio,
    Age_Median = yrs_pending.50,
    Age_75th_Percentile = yrs_pending.75,
    Age_95th_Percentile = yrs_pending.95,
    Age_Max = yrs_pending.max
  ) %>%
  write.csv("Pending NODs by RO.csv", row.names = FALSE)
