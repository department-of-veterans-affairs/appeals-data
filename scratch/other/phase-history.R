# A simplified approach to historical backlogs using events to signal phase transitions

source("R/vacolsConnect.R")

library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

# Outcome: BVA Resolved, ADV Resolved, Merged
# Stages: NOD, Form 9, Certified, Activated, Assigned, Post-Remand, Remand Returned, CAVC Remand
# Originating RO
# Last observed RO
# Caseflow
# Hearing date
# Issue resolutions
#   - Issue
#   - Disposition
#   - Remand reason

cases <- query("
  select
    BFCORLID,
    BFKEY,

    BFAC,
    BFSUB,
    BFDC,

    BFDDEC,
    BFDPDCN,

    BFDNOD,
    BFD19,
    BF41STAT,
    TIDRECV,
    TIOCTIME,
    MIN_DEASSIGN,

    MIN_HEARING_DATE,

    ADD_USER,
    ADD_LOC,
    ADD_DATE,

    X96_DATE,

    BFREGOFF,
    case when BFDCERTOOL is not null then 1 else 0 end as CASEFLOW

  from BRIEFF

  inner join (
    select distinct
      BFCORLID as D_BFCORLID,
      BFDNOD as D_BFDNOD
    from BRIEFF
    where BFDDEC is null or BFDDEC >= date '2013-10-01'
  ) on
    BFCORLID = D_BFCORLID and BFDNOD = D_BFDNOD

  inner join FOLDER
    on BFKEY = TICKNUM

  inner join (
    select
      PRIORLOC.LOCKEY as ADD_LOCKEY,
      PRIORLOC.LOCSTOUT as ADD_USER,
      PRIORLOC.LOCSTTO as ADD_LOC,
      PRIORLOC.LOCDOUT as ADD_DATE
    from PRIORLOC
    inner join (
      select LOCKEY, min(LOCDOUT) as MIN_LOCDOUT
      from PRIORLOC group by LOCKEY
    ) MINLOC on
      PRIORLOC.LOCKEY = MINLOC.LOCKEY
      and LOCDOUT = MIN_LOCDOUT
  ) on
    BFKEY = ADD_LOCKEY

  left join (
    select FOLDER_NR, min(HEARING_DATE) as MIN_HEARING_DATE
    from HEARSCHED
    join BRIEFF on FOLDER_NR = BFKEY
    where HEARING_DISP = 'H'
      and HEARING_TYPE in ('C', 'T', 'V')
      and HEARING_DATE >= BFD19
    group by FOLDER_NR
  ) on
    BFKEY = FOLDER_NR

  left join (
    select DEFOLDER, min(DEASSIGN) as MIN_DEASSIGN
    from DECASS
    group by DEFOLDER
  ) on
    BFKEY = DEFOLDER

  left join (
    select
      LOCKEY as X96_LOCKEY,
      min(LOCDOUT) as X96_DATE
    from PRIORLOC
    where LOCSTTO = '96'
    group by LOCKEY
  ) on
    BFKEY = X96_LOCKEY
") %>%
  group_by(BFKEY) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n == 1) %>%
  select(-n)

logical_case_ids <- cases %>%
  select(BFKEY, BFAC, BFCORLID, BFDNOD, BFDDEC, BFDPDCN)

logical_case_ids %<>%
  filter(BFAC == '1') %>%
  mutate(case_id = row_number()) %>%
  select(BFKEY, case_id) %>%
  right_join(logical_case_ids, by = c("BFKEY"))

logical_case_ids.na_cnt <- nrow(logical_case_ids)

while(logical_case_ids.na_cnt - sum(is.na(logical_case_ids$case_id)) > 0) {
  logical_case_ids.na_cnt <- sum(is.na(logical_case_ids$case_id))

  logical_case_ids %<>%
    filter(!is.na(case_id)) %>%
    select(BFCORLID, BFDNOD, BFDDEC, case_id) %>%
    right_join(logical_case_ids, by = c("BFCORLID", "BFDNOD", "BFDDEC" = "BFDPDCN")) %>%
    mutate(case_id = ifelse(is.na(case_id.y), case_id.x, case_id.y), BFDPDCN = BFDDEC, BFDDEC = BFDDEC.y) %>%
    select(-case_id.x, -case_id.y, -BFDDEC.y) %>%
    unique()
}

logical_case_ids %<>%
  select(BFKEY, case_id) %>%
  group_by(BFKEY) %>%
  filter(n() == 1, !is.na(case_id)) %>%
  ungroup()

cases <- logical_case_ids %>%
  inner_join(cases, by = c("BFKEY"))

issues <- query("
  select
    BFKEY,
    ISSUES.ISSSEQ,
    ISSUES.ISSDC,
    ISSUES.ISSDESC,
    ISSUES.ISSPROG,
    ISSUES.ISSCODE,
    ISSUES.ISSLEV1,
    ISSUES.ISSLEV2,
    ISSUES.ISSLEV3,
    ISSREF.PROG_DESC ISSPROG_LABEL,
    ISSREF.ISS_DESC ISSCODE_LABEL,
    case when ISSUES.ISSLEV1 is not null then
      case when ISSREF.LEV1_CODE = '##' then
        VFISSREF.FTDESC else ISSREF.LEV1_DESC
      end
    end ISSLEV1_LABEL,
    case when ISSUES.ISSLEV2 is not null then
      case when ISSREF.LEV2_CODE = '##' then
        VFISSREF.FTDESC else ISSREF.LEV2_DESC
      end
    end ISSLEV2_LABEL,
    case when ISSUES.ISSLEV3 is not null then
      case when ISSREF.LEV3_CODE = '##' then
        VFISSREF.FTDESC else ISSREF.LEV3_DESC
      end
    end ISSLEV3_LABEL,
    RMDREA.RMDVAL,
    VFRMDREA.FTDESC as RMD_LABEL

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

  left join (
    select * from VFTYPES
    where VFTYPES.FTTYPE = 'DG'
  ) VFISSREF on
    ((ISSREF.LEV1_CODE = '##' and 'DG' || ISSUES.ISSLEV1 = VFISSREF.FTKEY)
      or (ISSREF.LEV2_CODE = '##' and 'DG' || ISSUES.ISSLEV2 = VFISSREF.FTKEY)
      or (ISSREF.LEV3_CODE = '##' and 'DG' || ISSUES.ISSLEV3 = VFISSREF.FTKEY))

  left join RMDREA
    on ISSUES.ISSKEY = RMDREA.RMDKEY
    and ISSUES.ISSSEQ = RMDREA.RMDISSSEQ

  left join (
    select * from VFTYPES
    where FTTYPE in ('RR', 'RN', 'R5')
  ) VFRMDREA on
    'RR' || RMDREA.RMDVAL = VFRMDREA.FTKEY

  inner join BRIEFF
    on ISSUES.ISSKEY = BRIEFF.BFKEY

  inner join (
    select distinct
      BFCORLID as D_BFCORLID,
      BFDNOD as D_BFDNOD
    from BRIEFF
    where BFDDEC is null or BFDDEC >= date '2013-10-01'
  ) on
    BRIEFF.BFCORLID = D_BFCORLID and BRIEFF.BFDNOD = D_BFDNOD
")

issues <- logical_case_ids %>%
  inner_join(issues, by = c("BFKEY"))

# remands are one-to-many, and need to be broken out into their own dataframe.


