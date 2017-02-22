## What effect does a hearing have on whether an issue is allowed/remanded?

source("R/vacolsConnect.R")
library(dplyr)
library(ggplot2)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

issues <- query("
select
  ISSUES.ISSKEY,
  CORRES.SDOB,
  BRIEFF.BFSO,
  BRIEFF.BFD19,
  BRIEFF.BFDDEC,
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
      VFTYPES.FTDESC else ISSREF.LEV1_DESC
    end
  end ISSLEV1_LABEL,
  case when ISSUES.ISSLEV2 is not null then
    case when ISSREF.LEV2_CODE = '##' then
      VFTYPES.FTDESC else ISSREF.LEV2_DESC
    end
  end ISSLEV2_LABEL,
  case when ISSUES.ISSLEV3 is not null then
    case when ISSREF.LEV3_CODE = '##' then
      VFTYPES.FTDESC else ISSREF.LEV3_DESC
    end
  end ISSLEV3_LABEL,
  case when HEARSCHED.HEARING_DISP is not null then 1 else 0 end HEARING

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

inner join CORRES
  on BRIEFF.BFCORKEY = CORRES.STAFKEY

left join HEARSCHED
  on ISSUES.ISSKEY = HEARSCHED.FOLDER_NR
  and HEARSCHED.HEARING_TYPE in ('C', 'T', 'V')
  and HEARSCHED.HEARING_DISP = 'H'

where BRIEFF.BFDDEC >= date '2015-10-01'
  and BRIEFF.BFDDEC < date '2016-10-01'
  and BRIEFF.BFDC in ('1', '3', '4')
  and ISSUES.ISSDC in ('1', '3', '4')
  and BRIEFF.BFAC = '1'
  and ISSUES.ISSPROG = '02'
")

issues.by_disposition <- issues %>%
  select(-ISSKEY, -SDOB, -BFSO, -BFD19, -BFDDEC, -ISSSEQ, -ISSDESC) %>%
  group_by(ISSPROG, ISSCODE, ISSLEV1, ISSLEV2, ISSLEV3, ISSPROG_LABEL, ISSCODE_LABEL, ISSLEV1_LABEL, ISSLEV2_LABEL, ISSLEV3_LABEL) %>%
  summarize(
    hearing_n = sum(HEARING == 1),
    hearing_allowed = sum(HEARING == 1 & ISSDC == '1'),
    hearing_remanded = sum(HEARING == 1 & ISSDC == '3'),
    nonhearing_n = sum(HEARING == 0),
    nonhearing_allowed = sum(HEARING == 0 & ISSDC == '1'),
    nonhearing_remanded = sum(HEARING == 0 & ISSDC == '3')
  ) %>%
  ungroup() %>%
  filter(hearing_allowed > 0, hearing_remanded > 0, nonhearing_allowed > 0, nonhearing_remanded > 0) %>%
  mutate(
    total_n = hearing_n + nonhearing_n,
    total_allowed = hearing_allowed + nonhearing_allowed,
    total_remanded = hearing_remanded + nonhearing_remanded,
    percent_hearing = hearing_n / total_n,
    total_allowed_rate = total_allowed / total_n,
    total_remanded_rate = total_remanded / total_n,
    hearing_allowed_rate = hearing_allowed / hearing_n,
    hearing_remanded_rate = hearing_remanded / hearing_n,
    nonhearing_allowed_rate = nonhearing_allowed / nonhearing_n,
    nonhearing_remanded_rate = nonhearing_remanded / nonhearing_n,
    diff_allowed_rate = hearing_allowed_rate - nonhearing_allowed_rate,
    diff_remanded_rate = hearing_remanded_rate - nonhearing_remanded_rate,
    ratio_allowed_rate = hearing_allowed_rate / nonhearing_allowed_rate,
    ratio_remanded_rate = hearing_remanded_rate / nonhearing_remanded_rate,
    logit_allowed_rate = log(total_allowed_rate / (1 - total_allowed_rate)),
    logit_remanded_rate = log(total_remanded_rate / (1 - total_remanded_rate))
  )

weighted.mean(issues.by_disposition$nonhearing_allowed_rate, issues.by_disposition$total_n)
weighted.mean(issues.by_disposition$hearing_allowed_rate, issues.by_disposition$total_n)
weighted.mean(issues.by_disposition$nonhearing_remanded_rate, issues.by_disposition$total_n)
weighted.mean(issues.by_disposition$hearing_remanded_rate, issues.by_disposition$total_n)

issues.with_rates <- issues.by_disposition %>%
  select(ISSPROG, ISSCODE, ISSLEV1, ISSLEV2, ISSLEV3, logit_allowed_rate, logit_remanded_rate) %>%
  right_join(issues, by = c("ISSPROG", "ISSCODE", "ISSLEV1", "ISSLEV2", "ISSLEV3")) %>%
  mutate(
    age_at_form9 = as.numeric(as.Date(BFD19) - as.Date(SDOB)) / 365,
    private_attorney = BFSO == 'T',
    rep = as.factor(BFSO)
  )

qplot(data = issues.by_disposition, x = total_n, y = log(ratio_allowed_rate))

m1 <- glm(ISSDC == '1' ~ HEARING + logit_allowed_rate, family=binomial, data=issues.with_rates)

mean_total_allowed_rate <- weighted.mean(issues.by_disposition$total_allowed_rate, issues.by_disposition$total_n)
mean_logit_allowed_rate <- log(mean_total_allowed_rate / (1 - mean_total_allowed_rate))
mean_total_remanded_rate <- weighted.mean(issues.by_disposition$total_remanded_rate, issues.by_disposition$total_n)
mean_logit_remanded_rate <- log(mean_total_remanded_rate / (1 - mean_total_remanded_rate))

result <- predict(m1, data.frame(HEARING = c(1, 0), logit_allowed_rate = mean_logit_allowed_rate), type = "response", se.fit = TRUE)

paste0("Change in allowance rate: ", result$fit[1] - result$fit[2])
paste0("CI: ", sqrt(result$se.fit[1]^2 + result$se.fit[2]^2) * 1.96)

issues.by_disposition$nonhearing_expected_allowed_rate <- predict(m1, data.frame(HEARING = 0, logit_allowed_rate = issues.by_disposition$logit_allowed_rate), type = "response")
issues.by_disposition$hearing_expected_allowed_rate <- predict(m1, data.frame(HEARING = 1, logit_allowed_rate = issues.by_disposition$logit_allowed_rate), type = "response")

qplot(total_allowed_rate, hearing_expected_allowed_rate - nonhearing_expected_allowed_rate, data = issues.by_disposition)
qplot(
  x = total_allowed_rate,
  y = (hearing_expected_allowed_rate - nonhearing_expected_allowed_rate) - (hearing_allowed_rate - nonhearing_allowed_rate),
  alpha = total_n,
  data = issues.by_disposition
)
