source("R/vacolsConnect.R")
source("R/caseflowConnect.R")

library("dplyr")
library("glue")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }
cfCon <- caseflowConnect()
cfQuery <- function (query) { return(dbGetQuery(cfCon, query)) }

seam_cases <- query("
  select BFKEY, BFCORLID, (case when AOD_CNT is not null then 1 else 0 end) AOD
  from BRIEFF
  join FOLDER on BFKEY = TICKNUM
  left join (
    select count(*) AOD_CNT, TSKTKNM
    from ASSIGN
    where TSKACTCD in ('B', 'B1')
    group by TSKTKNM
  ) on BFKEY = TSKTKNM
  where BFDCERTOOL is not null
    and BF41STAT is not null
    and BFD19 is not null
    and BFHR = '5'
    and BFAC = '1'
    and BFMPRO = 'ADV'
    and (TISUBJ1 is null or TISUBJ1 <> 'Y')
")

v2_cases <- cfQuery("select vacols_id, v2 from certifications where completed_at is not null") %>%
  group_by(vacols_id) %>%
  summarize(v2 = any(v2)) %>%
  filter(v2)

intersect_cases <- seam_cases %>%
  inner_join(v2_cases, by = c('BFKEY' = 'vacols_id'))

for (bfkey in intersect_cases$BFKEY) {
  print(glue("activating {bfkey}"))

  bfmpro <- query(glue("select BFMPRO from BRIEFF where BFKEY = '{bfkey}'"))[1,1]

  if (bfmpro != "ADV") {
    print(glue("not in ADV status"))
    next
  }

  othcnt <- query(glue("select count(*) from OTHDOCS where TICKNUM = '{bfkey}'"))

  valid = TRUE

  if (othcnt == 0) {
    valid <- valid & query(glue("insert into OTHDOCS (TICKNUM, CLMFLD) values ('{bfkey}', '1')"))
  } else {
    valid <- valid & query(glue("update OTHDOCS set CLMFLD = '1' where TICKNUM = '{bfkey}'"))
  }

  valid <- valid & query(glue("update FOLDER set TICUKEY = 'ACTIVE', TIKEYWRD = 'ACTIVE', TIDRECV = SYSDATE where TICKNUM = '{bfkey}'"))

  valid <- valid & query(glue("update BRIEFF set BFMPRO = 'ACT', BFCURLOC = '03', BFDLOCIN = SYSDATE, BFDLOOUT = SYSDATE, BFORGTIC = null where BFKEY = '{bfkey}'"))

  valid <- valid & query(glue("update PRIORLOC set LOCDIN = SYSDATE, LOCSTRCV = 'DSUSER', LOCEXCEP = 'Y' where LOCKEY = '{bfkey}' and LOCDIN is null"))

  valid <- valid & query(glue("insert into PRIORLOC (LOCDOUT, LOCDTO, LOCSTTO, LOCSTOUT, LOCKEY) values (SYSDATE, SYSDATE, '03', 'DSUSER', '{bfkey}')"))

  if (valid) {
    valid <- valid & query("commit")
  }

  if (!valid) {
    print(glue("problem activating {bfkey}"))
    break
  }
}

write.csv(intersect_cases, "flag_flip_2017_12_28.csv", row.names = FALSE)
