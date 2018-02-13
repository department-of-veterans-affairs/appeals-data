source("R/vacolsConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")
library("ggplot2")
library("knitr")
library("lubridate")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

appeals <- query("
select BFKEY,
  BFCORLID,
  BFMPRO,
  BFDC,
  BFCURLOC,
  BFDNOD,
  BFDSOC,
  BFD19,
  BF41STAT,
  (case when VACOLS.AOD_CNT(BFKEY) > 0 then 1 else 0 end) AOD,
  SDOB,
  SSPARE2,
  (case when HEARING_CNT > 0 then 1 else 0 end) SCHEDULED_HEARING,
  COMP_ISSUE_CNT,
  (ISSUE_CNT - COMP_ISSUE_CNT) NONCOMP_ISSUE_CNT

from BRIEFF

inner join CORRES on STAFKEY = BFCORKEY

left join (
  select FOLDER_NR,
    count(*) HEARING_CNT

  from HEARSCHED

  where HEARING_TYPE in ('C', 'T', 'V')
    and HEARING_DISP is null

  group by FOLDER_NR
) HEARINGS
on FOLDER_NR = BFKEY

left join (
  select sum(case when ISSPROG = '02' then 1 else 0 end) COMP_ISSUE_CNT,
    count(*) ISSUE_CNT,
    ISSKEY
  from ISSUES
  group by ISSKEY
) ISSUES
on ISSKEY = BFKEY

where BFMPRO <> 'HIS' or BFDLOOUT >= date '2017-09-01'
")

appeal_cnt <- appeals %>% filter(BFMPRO != 'HIS') %>% count(BFCORLID)

nov_list <- read.csv("sensitive_data/the_500_take_2.csv", stringsAsFactors = FALSE, colClasses = c("character"), strip.white = TRUE) %>%
  left_join(appeals, by=c('folder' = 'BFKEY')) %>%
  left_join(appeal_cnt, by='BFCORLID') %>%
  mutate(
    not_found = is.na(BFCORLID),
    aod = AOD == 1,
    over_75 = is.na(SSPARE2) & as_date(SDOB) <= as_date("1942-12-01"),
    over_85 = is.na(SSPARE2) & as_date(SDOB) <= as_date("1932-12-01"),
    multiple_appeals = n > 1,
    scheduled_hearing = SCHEDULED_HEARING == 1,
    cert_ahead_of_docket = as_date(BFD19) <= as_date("2014-05-30") & !is.na(BF41STAT) & BFMPRO == 'ADV',
    eligible = (BFMPRO %in% c('ADV', 'REM') & BFCURLOC != '96') | is.na(BFDC) | BFDC == 'P',
    ineligible = !eligible
  ) %>%
  filter(!not_found)

dec_list <- read.csv("sensitive_data/ramp_dec_5000.csv", stringsAsFactors = FALSE, colClasses = c("character"), strip.white = TRUE) %>%
  transmute(
    id = FILE_NBR,
    first = FIRST_NAME,
    last = LAST_NAME,
    poa = POA_TYPE,
    stage = APPEAL_TYPE,
    in_stage = DAYS_IN_STAGE,
    folder = FOLDER_NBR
  ) %>%
  left_join(appeals, by=c('folder' = 'BFKEY')) %>%
  left_join(appeal_cnt, by='BFCORLID') %>%
  mutate(
    not_found = is.na(BFCORLID),
    aod = AOD == 1,
    over_75 = is.na(SSPARE2) & as_date(SDOB) <= as_date("1942-12-01"),
    over_85 = is.na(SSPARE2) & as_date(SDOB) <= as_date("1932-12-01"),
    multiple_appeals = n > 1,
    scheduled_hearing = SCHEDULED_HEARING == 1,
    cert_ahead_of_docket = as_date(BFD19) <= as_date("2014-05-30") & !is.na(BF41STAT) & BFMPRO == 'ADV',
    eligible = BFMPRO %in% c('ADV', 'REM') & BFCURLOC != '96',
    ineligible = !eligible
  ) %>%
  filter(!not_found)

sum(dec_list$multiple_appeals, na.rm = TRUE)
sum(dec_list$aod)
sum(dec_list$over_75, na.rm = TRUE)
sum(dec_list$over_85, na.rm = TRUE)
sum(dec_list$scheduled_hearing)
sum(dec_list$cert_ahead_of_docket, na.rm = TRUE)

sum(dec_list$ineligible)
table(dec_list$stage, dec_list$eligible)

sum(nov_list$ineligible)
table(nov_list$stage, nov_list$eligible)

jan_list <- read.csv("sensitive_data/ramp_jan_10000.csv", stringsAsFactors = FALSE, colClasses = c("character"), strip.white = TRUE) %>%
  transmute(
    id = FILE_NBR,
    first = FIRST_NAME,
    last = LAST_NAME,
    poa = POA_TYPE,
    stage = APPEAL_TYPE,
    in_stage = DAYS_IN_STAGE,
    folder = FOLDER_NBR
  ) %>%
  left_join(appeals, by=c('folder' = 'BFKEY')) %>%
  left_join(appeal_cnt, by='BFCORLID') %>%
  mutate(
    not_found = is.na(BFCORLID),
    aod = AOD == 1,
    over_75 = is.na(SSPARE2) & as_date(SDOB) <= as_date("1942-12-01"),
    over_85 = is.na(SSPARE2) & as_date(SDOB) <= as_date("1932-12-01"),
    multiple_appeals = n > 1,
    scheduled_hearing = SCHEDULED_HEARING == 1,
    cert_ahead_of_docket = as_date(BFD19) <= as_date("2014-05-30") & !is.na(BF41STAT) & BFMPRO == 'ADV',
    eligible = BFMPRO %in% c('ADV', 'REM') & BFCURLOC != '96',
    ineligible = !eligible
  )
