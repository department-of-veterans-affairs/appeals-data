source("R/vacolsConnect.R")

library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)
library(knitr)
library(scales)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

# Which locations have the most appeals
locs <- query("select BFCURLOC, count(*) N from BRIEFF where BFMPRO <> 'HIS' group by BFCURLOC order by N desc")
locs %>% filter(N >= 50) %>% kable()

# Which VSOs have the most appeals
vsos <- query("select BFSO, count(*) N from BRIEFF where BFCURLOC = '55' group by BFSO order by N desc")
vsos %>% kable()

# How long does an appeal sit at the various abeyances
abeyances <- query("select LOCSTTO, LOCDOUT, LOCDIN from PRIORLOC where LOCDIN > date '2016-09-14' and LOCSTTO in ('15', '18', '19', '24', '25')") %>%
  mutate(DURATION = as.numeric(as_date(LOCDIN) - as_date(LOCDOUT))) %>%
  group_by(LOCSTTO) %>%
  summarize(DURATION = median(DURATION))
abeyances %>% kable()

# How often does hearing transcription occur within 15 days?
hearings <- query("select CONSENT, CONRET from HEARSCHED where HEARING_TYPE in ('C', 'T', 'V') and HEARING_DISP = 'H' and CONSENT >= date '2016-08-30' and CONSENT < date '2017-08-30'") %>%
  mutate(win15 = !is.na(CONRET) & as.numeric(as_date(CONRET) - as_date(CONSENT)) <= 15)
sum(hearings$win15) / nrow(hearings)

# How often does translation occur within 30 days?
translation <- query("select LOCDOUT, LOCDIN from PRIORLOC where LOCDIN > date '2016-09-14' and LOCSTTO ='18'") %>%
  mutate(win30 = as.numeric(as_date(LOCDIN) - as_date(LOCDOUT)) <= 30)
sum(translation$win30) / nrow(translation)

# What locations hold the most AOD cases?
aod <- query("select BFCURLOC, count(*) N, sum(case when vacols.aod_cnt(BFKEY) > 0 then 1 else 0 end) AOD from BRIEFF where BFMPRO <> 'HIS' and BFCURLOC <> '77' and BFCURLOC <> '99' and length(BFCURLOC) <= 2 group by BFCURLOC") %>%
  mutate(AOD_RATE = AOD / N) %>%
  arrange(desc(AOD_RATE)) %>%
  mutate(AOD_RATE = percent(AOD_RATE))
kable(aod)

# What are the locations appeals sit at longest
durations <- query("select LOCSTTO, LOCDOUT, LOCDIN from PRIORLOC where LOCDIN > date '2016-09-14'") %>%
  mutate(DURATION = as.numeric(as_date(LOCDIN) - as_date(LOCDOUT))) %>%
  group_by(LOCSTTO) %>%
  summarize(N = n(), DURATION = median(DURATION)) %>%
  arrange(desc(DURATION)) %>%
  filter(N >= 10, DURATION >= 30)
durations %>% kable()
