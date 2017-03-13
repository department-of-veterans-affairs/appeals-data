# How often are various fields changed on the form 8?

source("R/caseflowConnect.R")
library(dplyr)
library(ggplot2)

con <- caseflowConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

form8s <- query("
select
  FORM8S.VACOLS_ID,
  APPELLANT_NAME, _INITIAL_APPELLANT_NAME,
  APPELLANT_RELATIONSHIP, _INITIAL_APPELLANT_RELATIONSHIP,
  VETERAN_NAME, _INITIAL_VETERAN_NAME,
  INSURANCE_LOAN_NUMBER, _INITIAL_INSURANCE_LOAN_NUMBER,
  SERVICE_CONNECTION_NOTIFICATION_DATE, _INITIAL_SERVICE_CONNECTION_NOTIFICATION_DATE,
  INCREASED_RATING_NOTIFICATION_DATE, _INITIAL_INCREASED_RATING_NOTIFICATION_DATE,
  OTHER_NOTIFICATION_DATE, _INITIAL_OTHER_NOTIFICATION_DATE,
  REPRESENTATIVE_NAME, _INITIAL_REPRESENTATIVE_NAME,
  REPRESENTATIVE_TYPE, _INITIAL_REPRESENTATIVE_TYPE,
  HEARING_REQUESTED, _INITIAL_HEARING_REQUESTED,
  SSOC_REQUIRED, _INITIAL_SSOC_REQUIRED

from FORM8S

join (select CERTIFICATION_ID ID, max(CREATED_AT) DATE from FORM8S group by CERTIFICATION_ID) LAST
  on CERTIFICATION_ID = LAST.ID
  and CREATED_AT = LAST.DATE

join CERTIFICATIONS
  on CERTIFICATION_ID = CERTIFICATIONS.ID

where FORM8S.VACOLS_ID is not null
  and CERTIFICATIONS.COMPLETED_AT is not null
")

fields <- form8s %>%
  mutate_all(as.character) %>%
  gather(key, value, -vacols_id) %>%
  mutate(col = ifelse(grepl("_initial_", key), "initial", "final"), key = gsub("_initial_", "", key)) %>%
  replace_na(list(value = "NULL")) %>%
  spread(col, value) %>%
  mutate(changed = initial != final)

table(fields$key, fields$changed)
round(prop.table(table(fields$key, fields$changed), 1), 2)

table(fields$key[fields$changed == TRUE & fields$initial == "NULL"])

table(fields$initial[fields$key == "hearing_requested" & fields$changed == TRUE])
table(fields$initial[fields$key == "hearing_requested"])

table(fields$initial[fields$key == "representative_name" & fields$changed == TRUE])

table(fields$initial[fields$key == "representative_type" & fields$changed == TRUE])
table(fields$final[fields$key == "representative_type" & fields$changed == TRUE & fields$initial == "Organization"])
