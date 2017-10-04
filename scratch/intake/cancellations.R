source("R/caseflowConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")

cfCon <- caseflowConnect()
cfQuery <- function (query) { return(dbGetQuery(cfCon, query)) }

cfQuery("select vacols_id, created_at, cancellation_reason, other_reason, email from certifications join certification_cancellations on certifications.id = certification_id where certifications.v2 = true") %>%
  View()
