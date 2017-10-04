source("R/vacolsConnect.R")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

issues <- query("
select VFTYPES.FTDESC, count(*)

from ISSUES

inner join ISSREF
  on ISSUES.ISSPROG = ISSREF.PROG_CODE
  and ISSUES.ISSCODE = ISSREF.ISS_CODE
  and (ISSUES.ISSLEV1 is null
    or ISSREF.LEV1_CODE = '##'
    or ISSUES.ISSLEV1 = ISSREF.LEV1_CODE)
  and (ISSUES.ISSLEV2 is null
    or ISSREF.LEV2_CODE = '##'
    or ISSUES.ISSLEV2 = ISSREF.LEV2_CODE)
  and (ISSUES.ISSLEV3 is null
    or ISSREF.LEV3_CODE = '##'
    or ISSUES.ISSLEV3 = ISSREF.LEV3_CODE)

left join VFTYPES
  on VFTYPES.FTTYPE = 'DG'
  and ((ISSREF.LEV1_CODE = '##' and 'DG' || ISSUES.ISSLEV1 = VFTYPES.FTKEY)
    or (ISSREF.LEV2_CODE = '##' and 'DG' || ISSUES.ISSLEV2 = VFTYPES.FTKEY)
    or (ISSREF.LEV3_CODE = '##' and 'DG' || ISSUES.ISSLEV3 = VFTYPES.FTKEY))

inner join BRIEFF
  on ISSUES.ISSKEY = BRIEFF.BFKEY

where BRIEFF.BFDNOD >= date '2016-10-01'
  and BRIEFF.BFDNOD < date '2017-04-01'
  and BRIEFF.BFAC = '1'
  and BRIEFF.BFMPRO = 'ADV'
  and BRIEFF.BFDSOC is null
  and ISSUES.ISSPROG = '02'
  and VFTYPES.FTDESC is not null

group by VFTYPES.FTDESC

order by count(*) desc
")
