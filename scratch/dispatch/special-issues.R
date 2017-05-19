# Do decisions made in Dispatch match what's expected based on VACOLS data?

source("R/vacolsConnect.R")
source("R/caseflowConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")
library("ggplot2")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }
cfCon <- caseflowConnect()
cfQuery <- function (query) { return(dbGetQuery(cfCon, query)) }

eps <- cfQuery("
  select
    CSS_ID,
    VACOLS_ID,
    COMPLETION_STATUS,
    RICE_COMPLIANCE,
    PRIVATE_ATTORNEY_OR_AGENT,
    WAIVER_OF_OVERPAYMENT,
    PENSION_UNITED_STATES,
    VAMC,
    INCARCERATED_VETERANS,
    DIC_DEATH_OR_ACCRUED_BENEFITS_UNITED_STATES as DIC_DEATH_OR_ACCRUED_BENEFITS,
    VOCATIONAL_REHAB,
    MANLINCON_COMPLIANCE,
    HEARING_INCLUDING_TRAVEL_BOARD_VIDEO_CONFERENCE as HEARING,
    HOME_LOAN_GUARANTY,
    INSURANCE,
    NATIONAL_CEMETERY_ADMINISTRATION as NCA,
    SPINA_BIFIDA,
    RADIATION,
    NONRATING_ISSUE,
    US_TERRITORY_CLAIM_PHILIPPINES,
    CONTAMINATED_WATER_AT_CAMP_LEJEUNE as CAMP_LEJEUNE,
    MUSTARD_GAS,
    EDUCATION_GI_BILL_DEPENDENTS_EDUCATIONAL_ASSISTANCE_SCHOLARS as EDUCATION_GI_BILL

  from TASKS

  join APPEALS
    on APPEAL_ID = APPEALS.ID

  join USERS on USER_ID = USERS.ID

  where COMPLETION_STATUS in ('0', '3', '5', '7')
    and COMPLETED_AT >= '2017-04-13'::date
") %>%
  mutate(
    completion_status = factor(completion_status, levels = 0:8, labels = c(
      'routed_to_arc',
      'canceled',
      'expired',
      'routed_to_ro',
      'assigned_existing_ep',
      'special_issue_emailed',
      'special_issue_not_emailed',
      'special_issue_vacols_routed',
      'invalidated'
    ))
  )

decisions <- query("
  select
    t1.*,
    PRIVATE_ATTORNEY_OR_AGENT,
    US_TERRITORY_CLAIM_PHILIPPINES,
    CAMP_LEJEUNE,
    RADIATION,
    MUSTARD_GAS,
    INCARCERATED_VETERANS
  from
  (
    select
      BFKEY,

      -- ** ISSUES ** --

      -- Education --
      case when count(case when
        ISSPROG = '03'
      then 1 end) > 0 then 1 else 0 end as EDUCATION_GI_BILL,

      -- Insurance --
      case when count(case when
        ISSPROG = '04'
      then 1 end) > 0 then 1 else 0 end as INSURANCE,

      -- Home Loan Guaranty --
      case when count(case when
        ISSPROG = '05'
      then 1 end) > 0 then 1 else 0 end as HOME_LOAN_GUARANTY,

      -- VAMC/Medical --
      case when count(case when
        ISSPROG = '06'
      then 1 end) > 0 then 1 else 0 end as VAMC,

      -- Pension --
      case when count(case when
        ISSPROG = '07'
      then 1 end) > 0 then 1 else 0 end as PENSION_UNITED_STATES,

      -- VR&E --
      case when count(case when
        ISSPROG = '08'
      then 1 end) > 0 then 1 else 0 end as VOCATIONAL_REHAB,

      -- National Cemetery Administration --
      case when count(case when
        ISSPROG = '11'
      then 1 end) > 0 then 1 else 0 end as NCA,

      -- Competency of Payee --
      case when count(case when
        ISSPROG = '02' and ISSCODE = '06'
      then 1 end) > 0 then 1 else 0 end as EPTEST_COMPETENCY,

      -- DIC/Death --
      case when count(case when
        (ISSPROG = '02' and ISSCODE = '08') or
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
      then 1 end) > 0 then 1 else 0 end as DIC_DEATH_OR_ACCRUED_BENEFITS,

      -- Attorney Fees --
      case when count(case when
        ISSPROG = '09' and ISSCODE = '01'
      then 1 end) > 0 then 1 else 0 end as EPTEST_ATTORNEYFEES,

      -- Spina Bifida --
      case when count(case when
        ISSPROG = '09' and ISSCODE = '03'
      then 1 end) > 0 then 1 else 0 end as SPINA_BIFIDA,

      -- Overpayment --
      case when count(case when
        (ISSPROG = '02' and ISSCODE = '13' and ISSLEV1 = '02') or
        (ISSPROG = '03' and ISSCODE = '05' and ISSLEV1 = '02') or
        (ISSPROG = '07' and ISSCODE = '10' and ISSLEV1 = '02')
      then 1 end) > 0 then 1 else 0 end as WAIVER_OF_OVERPAYMENT,

      -- Non-Rating Issues --
      case when count(case when
        (ISSPROG = '02' and ISSCODE = '16') or
        (ISSPROG = '02' and ISSCODE = '21') or
        (ISSPROG = '02' and ISSCODE = '22')
      then 1 end) > 0 then 1 else 0 end as NONRATING_ISSUE,

      -- ** REMANDS ** --

      -- Due Process: BVA Travel Board/Video Hearing --
      case when count(case when
        RMDVAL = 'EA'
      then 1 end) > 0 then 1 else 0 end as HEARING,

      -- Manlincon Compliance --
      case when count(case when
        RMDVAL = 'ED'
      then 1 end) > 0 then 1 else 0 end as MANLINCON_COMPLIANCE,

      -- Rice Compliance --
      case when count(case when
        ISSPROG = '02' and ISSCODE = '17' and RMDVAL = 'AA'
      then 1 end) > 0 then 1 else 0 end as RICE_COMPLIANCE

    from
      BRIEFF
      left join ISSUES on BRIEFF.BFKEY = ISSUES.ISSKEY
      left join RMDREA on ISSUES.ISSKEY = RMDREA.RMDKEY and ISSUES.ISSSEQ = RMDREA.RMDISSSEQ

    group by BFKEY
  ) t1
  join
  (
    select
      BFKEY,
      BFDC,
      BFDDEC,

      -- ** DOCKET ** --

      -- Private Attorney --
      case when
        BFSO in ('T', 'U', 'Z')
      then 1 else 0 end as PRIVATE_ATTORNEY_OR_AGENT,

      -- Manila --
      case when
        BFREGOFF = 'RO58'
      then 1 else 0 end as US_TERRITORY_CLAIM_PHILIPPINES,

      -- ** SPECIAL INTERESTS ** --

      -- Camp Legune Contaminated Water --
      case when
        TICLCW = 'Y'
      then 1 else 0 end as CAMP_LEJEUNE,

      -- Radiation --
      case when
        TIRADB = 'Y' or TIRADN = 'Y'
      then 1 else 0 end as RADIATION,

      -- Mustard Gas --
      case when
        TIMGAS = 'Y'
      then 1 else 0 end as MUSTARD_GAS,

      -- Incarceration --
      case when
        SINCAR = 'Y'
      then 1 else 0 end as INCARCERATED_VETERANS

    from
      BRIEFF
      left join CORRES on BRIEFF.BFCORKEY = CORRES.STAFKEY
      left join FOLDER on BRIEFF.BFKEY = FOLDER.TICKNUM
  ) t2
  on t1.BFKEY = t2.BFKEY
  where BFDDEC >= date '2017-04-01'
    and BFDC in ('1', '3')
") %>%
  mutate(any_issue = ifelse(rowSums(.[2:length(.)]) > 0, 1, 0)) %>%
  mutate_each(funs(as.logical), -BFKEY)

special_issue_pairs <- decisions %>%
  gather(issue, present, -BFKEY, -any_issue) %>%
  mutate(issue = tolower(issue)) %>%
  inner_join(eps %>% gather(issue, present, -vacols_id, -completion_status, -css_id), by = c("BFKEY" = "vacols_id", "issue")) %>%
  select(BFKEY, issue, completion_status, predict = present.x, actual = present.y, any_issue, css_id) %>%
  mutate(
    true_positive = predict & actual,
    true_negative = !predict & !actual,
    false_positive = predict & !actual,
    false_negative = !predict & actual,
    catastrophic_positive = predict & !actual & completion_status == 'routed_to_arc',
    catastrophic_negative = !predict & actual & !any_issue
  )

by_issue <- special_issue_pairs %>%
  group_by(issue) %>%
  summarize_at(vars(true_positive, true_negative, false_positive, false_negative, catastrophic_positive, catastrophic_negative), sum) %>%
  mutate(positive_error = false_positive / (true_positive + false_positive))

by_user <- special_issue_pairs %>%
  group_by(css_id) %>%
  summarize_at(vars(true_positive, true_negative, false_positive, false_negative, catastrophic_positive, catastrophic_negative), sum) %>%
  mutate(positive_error = false_positive / (true_positive + false_positive))
