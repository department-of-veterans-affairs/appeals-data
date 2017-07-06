# Ensure VACOLS is consistent with Caseflow DB following an event

source("R/vacolsConnect.R")
source("R/caseflowConnect.R")

library("dplyr")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }
cfCon <- caseflowConnect()
cfQuery <- function (query) { return(dbGetQuery(cfCon, query)) }

# Certification

caseflow <- cfQuery("
  select vacols_id
  from certifications
  where completed_at >= '2017-07-06'::date
    and completed_at < '2017-07-06'::date + '1 day'::interval
")

vacols <- query(paste0("select BFKEY, BF41STAT, BFDCERTOOL from BRIEFF where BFKEY in ('", paste(caseflow$vacols_id, collapse = "','"), "')"))

nrow(caseflow) == nrow(vacols)
sum(is.na(vacols$BF41STAT)) == 0
sum(is.na(vacols$BFDCERTOOL)) == 0

# Dispatch

caseflow <- cfQuery("
  select vacols_id, completion_status
  from tasks
  join appeals on appeal_id = appeals.id
  where completion_status in (0, 3)
    and completed_at >= '2017-07-06'::date
    and completed_at < '2017-07-06'::date + '1 day'::interval
")

vacols <- query(paste0("select BFKEY, BFCURLOC from BRIEFF where BFKEY in ('", paste(caseflow$vacols_id, collapse = "','"), "')"))

nrow(caseflow) == nrow(vacols)
sum(vacols$BFCURLOC != '97') == nrow(vacols)

joined <- caseflow %>%
  inner_join(vacols, by = c("vacols_id" = "BFKEY")) %>%
  filter(BFCURLOC != '99') %>%
  mutate(correct = (completion_status == 0 & BFCURLOC == '98') | (completion_status == 3 & BFCURLOC == '50'))

# This one might return FALSE if something's been moved after it's been dispatched. Use the following to confirm:
# query("select * from PRIORLOC where LOCKEY = 'XXXXXXX' order by LOCDOUT")

sum(joined$correct) == nrow(joined)
