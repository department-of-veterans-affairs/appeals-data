# Hearings by timezone

source("R/vacolsConnect.R")

library("dplyr")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

ros <- read.csv('data/ro.csv', stringsAsFactors = FALSE)

hearings.fy16 <- query("select BFREGOFF, count(*) N from HEARSCHED join BRIEFF on FOLDER_NR = BFKEY where HEARING_TYPE in ('T', 'V') and HEARING_DISP = 'H' and HEARING_DATE >= date '2015-10-01' and HEARING_DATE < date '2016-10-01' group by BFREGOFF") %>%
  inner_join(ros, by = c("BFREGOFF")) %>%
  group_by(Timezone) %>%
  summarize(FY16_Held = sum(N))

hearings.pending <- query("select BFREGOFF, count(*) N from BRIEFF left join (select FOLDER_NR, count(*) HEARINGS from HEARSCHED where HEARING_TYPE in ('C', 'T', 'V') and HEARING_DISP in ('H', 'C', 'N') group by FOLDER_NR) on BFKEY = FOLDER_NR where BFHR in ('1', '2') and HEARINGS is null and BFREGOFF is not null and BFMPRO = 'ADV' group by BFREGOFF") %>%
  inner_join(ros, by = c("BFREGOFF")) %>%
  group_by(Timezone) %>%
  summarize(Pending_Hearing = sum(N))
