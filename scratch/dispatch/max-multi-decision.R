# How many decision documents might match for a given decision? https://github.com/department-of-veterans-affairs/caseflow/issues/1273

source("R/vacolsConnect.R")
library(dplyr)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

# def decisions
#   decisions = documents_with_type("BVA Decision").select do |decision|
#     (decision.received_at.in_time_zone - decision_date).abs <= 3.days
#   end
#   decisions
# end

decisions <- query("select BFCORLID, BFDDEC from BRIEFF where BFDC between '1' and '9' and BFAC in ('1', '3') and BFDDEC >= date '2013-10-01' and BFDDEC < date '2016-10-01'") %>%
  mutate(BFDDEC = as.Date(BFDDEC))

decisions$matches <- apply(decisions, 1, function(x) sum(decisions$BFCORLID == x['BFCORLID'] & abs(decisions$BFDDEC - as.Date(x['BFDDEC'])) <= 3))

summary(decisions$matches)
table(decisions$matches)
