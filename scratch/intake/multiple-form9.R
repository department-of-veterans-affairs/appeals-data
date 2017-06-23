# How frequently does a Veteran have multiple Form 9s filed on the same date?
# https://github.com/department-of-veterans-affairs/caseflow/issues/2427

source("R/vacolsConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

cases <- query("select BFCORLID, BFD19 from BRIEFF where BFD19 is not null and BF41STAT is null and BFMPRO = 'ADV'")

n_vets <- length(unique(cases$BFCORLID))

n_multi9 <- cases %>%
  count(BFCORLID, BFD19) %>%
  filter(n > 1) %>%
  nrow()

n_multi9 / n_vets
