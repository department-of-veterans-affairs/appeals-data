# What percentage of decisions involve a DIC or accrued issue? https://github.com/department-of-veterans-affairs/appeals-pm/issues/1112

source("R/vacolsConnect.R")
source("R/events.R")
library(dplyr)

con <- vacolsConnect()

decisions <- dbGetQuery(con, "
  select BFKEY,

  count(case when
  ISSDC = '1'
  then 1 end) as ALLOWED,

  count(case when
  ISSDC = '3'
  then 1 end) as REMANDED,

  count(case when
  ISSDC = '4'
  then 1 end) as DENIED,

  count(case when
  ISSPROG = '02' and ISSCODE = '08'
  then 1 end) as DIC,

  count(case when
  (ISSPROG = '02' and ISSCODE = '01' and ISSLEV1 = '01') or
  (ISSPROG = '02' and ISSCODE = '07' and ISSLEV1 = '01') or
  (ISSPROG = '02' and ISSCODE = '09' and ISSLEV1 = '01') or
  (ISSPROG = '02' and ISSCODE = '11' and ISSLEV1 = '01') or
  (ISSPROG = '02' and ISSCODE = '12' and ISSLEV1 = '02') or
  (ISSPROG = '02' and ISSCODE = '14' and ISSLEV1 = '01') or
  (ISSPROG = '02' and ISSCODE = '15' and ISSLEV1 = '01') or
  (ISSPROG = '02' and ISSCODE = '17' and ISSLEV1 = '01') or
  (ISSPROG = '02' and ISSCODE = '18' and ISSLEV1 = '01' and ISSLEV2 = '01') or
  (ISSPROG = '02' and ISSCODE = '20' and ISSLEV1 = '01') or
  (ISSPROG = '03' and ISSCODE = '01') or
  (ISSPROG = '07' and ISSCODE = '01')
  then 1 end) as ACCRUED

  from BRIEFF
  left join ISSUES on BRIEFF.BFKEY = ISSUES.ISSKEY
  left join RMDREA on ISSUES.ISSKEY = RMDREA.RMDKEY and ISSUES.ISSSEQ = RMDREA.RMDISSSEQ

  where BFDDEC >= date '2015-10-01' and BFDDEC < date '2016-10-01'

  group by BFKEY
") %>%
  mutate(
    disposition = factor(
      ifelse(ALLOWED > 0,
        ifelse(REMANDED > 0, "Partial Grant", "Full Grant"),
        ifelse(REMANDED > 0, "Remand", "Denial")
      ), levels = c("Full Grant", "Partial Grant", "Remand", "Denial")),
    has_dic_issue = DIC > 0,
    has_accrued_issue = ACCRUED > 0
  )

sum(decisions$has_dic_issue & decisions$disposition != "Denial") / sum(decisions$disposition != "Denial")
table(decisions$disposition[decisions$has_dic_issue]) / table(decisions$disposition)
sum(decisions$has_accrued_issue & decisions$disposition != "Denial") / sum(decisions$disposition != "Denial")
table(decisions$disposition[decisions$has_accrued_issue]) / table(decisions$disposition)
