## How often does a new and material issue on a comp claim sway the disposition?

source("R/vacolsConnect.R")
library(dplyr)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

cases <- query("
  select BFKEY,

  count(case when ISSDC = '1' then 1 end) as ALLOWED,

  count(case when ISSDC = '3' then 1 end) as REMANDED,

  count(case when
  ISSPROG = '02' and ISSCODE = '15' and ISSLEV1 = '04'
  then 1 end) as NM,

  count(case when
  ISSPROG = '02' and ISSCODE = '15' and ISSLEV1 = '04' and ISSDC = '1'
  then 1 end) as ALLOWED_NM,

  count(case when
  ISSPROG = '02' and ISSCODE = '15' and ISSLEV1 = '04' and ISSDC = '3'
  then 1 end) as REMANDED_NM

  from BRIEFF
  left join ISSUES on BRIEFF.BFKEY = ISSUES.ISSKEY

  where BFDDEC >= date '2015-10-01' and BFDDEC < date '2016-10-01'
    and BFDC in ('1', '3')

  group by BFKEY
") %>%
  mutate(
    is_allowed = ALLOWED > 0,
    but_not_grant = is_allowed & ALLOWED - ALLOWED_NM <= 0
  )

sum(cases$but_not_grant) / nrow(cases)
sum(cases$but_not_grant) / sum(cases$is_allowed)
