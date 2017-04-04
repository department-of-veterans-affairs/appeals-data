source("R/caseflowConnect.R")
source("R/vacolsConnect.R")
library(dplyr)

caseflowCon <- caseflowConnect()
caseflowQuery <- function (query) { return(dbGetQuery(caseflowCon, query)) }

vacolsCon <- vacolsConnect()
vacolsQuery <- function (query) { return(dbGetQuery(vacolsCon, query)) }

completions <- caseflowQuery("select VACOLS_ID, COMPLETED_AT from TASKS join APPEALS on APPEAL_ID = APPEALS.ID where COMPLETION_STATUS = 0")
outcodings <- vacolsQuery("select TICKNUM, TIOCTIME from FOLDER where TIOCTIME > date '2017-02-01'")

joined <- completions %>%
  left_join(outcodings, by = c("vacols_id" = "TICKNUM")) %>%
  mutate(time_to_ep = as.numeric(as.Date(completed_at) - as.Date(TIOCTIME)))
