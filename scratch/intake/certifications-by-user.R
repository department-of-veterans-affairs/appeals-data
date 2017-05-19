# Certifications by user

source("R/caseflowConnect.R")

library("dplyr")

cfCon <- caseflowConnect()
cfQuery <- function (query) { return(dbGetQuery(cfCon, query)) }

cfQuery("select users.*, completed_at from certifications join users on user_id = users.id where completed_at is not null") %>%
  mutate(completed_at = as.Date(completed_at)) %>%
  count()
