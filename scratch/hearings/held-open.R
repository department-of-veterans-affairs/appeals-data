# How often to VLJs opt to hold an appeal open after hearing/does it have an effect on outcomes?

source("R/vacolsConnect.R")

library("dplyr")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

holddays <- query("select HOLDDAYS from HEARSCHED where HEARING_TYPE in ('C', 'T', 'V') and HEARING_DISP = 'H' and HEARING_DATE >= date '2015-10-01' and HEARING_DATE < date '2016-10-01'") %>%
  replace_na(list(HOLDDAYS = 0))
table(holddays)

heldissues <- query("
select ISSDC,
  MAXHOLDDAYS

from ISSUES

join (
  select FOLDER_NR,
    max(case when HOLDDAYS is null then 0 else HOLDDAYS end) MAXHOLDDAYS

  from HEARSCHED
  where HEARING_TYPE in ('C', 'T', 'V')
    and HEARING_DISP = 'H'

  group by FOLDER_NR
) on ISSKEY = FOLDER_NR

join BRIEFF on ISSKEY = BFKEY

where BFDDEC >= date '2015-10-01'
  and BFDDEC < date '2016-10-01'
  and BFAC = '1'
  and BFDC in ('1', '3', '4')
  and ISSDC in ('1', '3', '4')
  and ISSPROG = '02'
") %>%
  mutate(held_open = MAXHOLDDAYS > 0) %>%
  group_by(held_open) %>%
  summarize(
    n_total = n(),
    n_allowed = sum(ISSDC == '1'),
    n_remanded = sum(ISSDC == '3'),
    allowance_rate = n_allowed / n_total,
    remand_rate = n_remanded / n_total
  )
