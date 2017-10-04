# Categorizing appeals in loc 97 for AMO

source("R/vacolsConnect.R")
source("R/caseflowConnect.R")

library("dplyr")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }
cfCon <- caseflowConnect()
cfQuery <- function (query) { return(dbGetQuery(cfCon, query)) }

abby97 <- read.csv("sensitive_data/abby_dispatch_97.csv", stringsAsFactors = FALSE) %>%
  mutate(BFKEY = as.character(BFKEY)) %>%
  left_join(query(paste0("
    select BFKEY, BFCORLID, BFMPRO, BFCURLOC, BFDDEC, TIOCTIME, TIVBMS
    from BRIEFF
    join FOLDER on BFKEY = TICKNUM
    where BFKEY in ('", paste(abby97$BFKEY, collapse = "','"), "')")), by = c("BFKEY")) %>%
  left_join(cfQuery(paste0("
    select VACOLS_ID, AASM_STATE, COMPLETION_STATUS, COMPLETED_AT from TASKS join APPEALS on APPEAL_ID = APPEALS.ID
    where (COMPLETION_STATUS is null or COMPLETION_STATUS <> 2)
      and VACOLS_ID in ('", paste(abby97$BFKEY, collapse = "','"), "')")), by = c("BFKEY" = "vacols_id")) %>%
  mutate(
    completion_status = factor(completion_status, levels = 0:8, labels = c(
      'routed_to_arc',
      'canceled',
      'expired',
      'routed_to_ro',
      'assigned_existing_ep',
      'special_issue_emailed',
      'special_issue_not_emailed',
      'special_issue_vacols_routed',
      'invalidated'
    ))
  ) %>%
  replace_na(list(
    TIVBMS = 'N',
    aasm_state = "not_in_caseflow"
  ))

table(abby97$aasm_state)

table(subset(abby97, aasm_state == 'completed')$completion_status)

table(subset(abby97, aasm_state == 'not_in_caseflow')$TIVBMS == 'Y')

table(subset(abby97, aasm_state == 'not_in_caseflow' & TIVBMS == 'Y')$TIOCTIME)

abby97.cancelled <- subset(abby97, completion_status == 'canceled')

write.csv(abby97.cancelled, 'cancelled.csv')



loc97 <- query("
  select BFKEY, BFCORLID, BFMPRO, BFCURLOC, BFDDEC, TIOCTIME, TIVBMS, BFSO
  from BRIEFF
  join FOLDER on BFKEY = TICKNUM
  where BFCURLOC = '97'
")


loc97 <- query("
  select BFKEY, BFCORLID, SNAMEF, SNAMEL, BFMPRO, BFCURLOC, BFDDEC, TIOCTIME, TIVBMS, BFSO, BFDC, GRANTS, REMANDS
  from BRIEFF
  join FOLDER on BFKEY = TICKNUM
  join CORRES on BFCORKEY = STAFKEY
  join (
    select ISSKEY,
      sum(case when (ISSPROG <> '02' or ISSCODE <> '15' or ISSLEV1 <> '04') and ISSDC = '1' then 1 else 0 end) GRANTS,
      sum(case when ISSDC = '3' then 1 else 0 end) REMANDS
    from ISSUES
    group by ISSKEY
  ) on BFKEY = ISSKEY
  where TIOCTIME >= date '2017-06-20' and TIOCTIME < date '2017-07-22' and (GRANTS > 0 or REMANDS > 0)
")

joined <- loc97 %>%
  left_join(cfQuery(paste0("
    select VACOLS_ID, AASM_STATE, COMPLETION_STATUS, COMPLETED_AT from TASKS join APPEALS on APPEAL_ID = APPEALS.ID
    where (COMPLETION_STATUS is null or COMPLETION_STATUS <> 2)
      and VACOLS_ID in ('", paste(loc97$BFKEY, collapse = "','"), "')")), by = c("BFKEY" = "vacols_id")) %>%
  mutate(
    completion_status = factor(completion_status, levels = 0:8, labels = c(
      'routed_to_arc',
      'canceled',
      'expired',
      'routed_to_ro',
      'assigned_existing_ep',
      'special_issue_emailed',
      'special_issue_not_emailed',
      'special_issue_vacols_routed',
      'invalidated'
    ))
  ) %>%
  replace_na(list(
    TIVBMS = 'N',
    aasm_state = "not_in_caseflow"
  ))

remands <- subset(joined, REMANDS > 0)

table(joined$aasm_state)

table(subset(joined, aasm_state == 'completed')$completion_status)

table(subset(joined, aasm_state == 'not_in_caseflow' & BFSO != 'T')$TIVBMS == 'Y')

table(subset(joined, aasm_state == 'not_in_caseflow' & TIVBMS == 'Y' & REMANDS == 0)$TIOCTIME)

loc97.cancelled <- subset(joined, completion_status == 'canceled')
