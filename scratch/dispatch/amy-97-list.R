# Are there problems identified by the list of appeals sent by Amy Chelgreen on 4/12?

source("R/vacolsConnect.R")
source("R/caseflowConnect.R")

library("dplyr")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

cfcon <- caseflowConnect()
cfquery <- function (query) { return(dbGetQuery(cfcon, query)) }

amy <- read.csv('sensitive_data/amy_list.csv', stringsAsFactors = FALSE, strip.white = TRUE)

vacols <- query(paste0("select BFKEY, BFDDEC, TIOCTIME, BFCURLOC from BRIEFF join FOLDER on BFKEY = TICKNUM where BFKEY in ('", paste(amy$ticknum, collapse = "','"), "')"))

caseflow <- cfquery("select vacols_id, created_at, assigned_at, started_at, completed_at, completion_status, aasm_state from tasks join appeals on appeal_id = appeals.id where type = 'EstablishClaim'")

combined <- amy %>%
  left_join(vacols, by = c("ticknum" = "BFKEY")) %>%
  left_join(caseflow, by = c("ticknum" = "vacols_id"))

table(combined$aasm_state, useNA = "ifany")
table(combined$completion_status)
table(combined$BFCURLOC[combined$completion_status != 0])
sum(is.na(combined$aasm_state) & combined$tivbms != 'Y')
