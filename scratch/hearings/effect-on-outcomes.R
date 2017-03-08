## What effect does a hearing have on whether an issue is allowed/remanded?

source("R/vacolsConnect.R")
library(dplyr)
library(magrittr)
library(lme4)
library(modelr)
library(purrr)
library(psych)
library(sjstats)
library(pROC)
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
  case when HEARSCHED.HEARING_DISP is not null then 1 else 0 end HEARING,
  HEARSCHED.HEARING_TYPE

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
  and HEARSCHED.HEARING_DATE < BRIEFF.BFDDEC

where BRIEFF.BFDDEC >= date '2015-10-01'
  and BRIEFF.BFDDEC < date '2016-10-01'
  and BRIEFF.BFDC in ('1', '3', '4')
  and ISSUES.ISSDC in ('1', '3', '4')
  and BRIEFF.BFAC = '1'
  and ISSUES.ISSPROG = '02'
") %>%
  mutate(
    hearing_typef = factor(ifelse(HEARING == 0, "none", ifelse(HEARING_TYPE == "V", "video", "in-person")), c("none", "video", "in-person")),
    age_at_form9 = as.numeric(as.Date(BFD19) - as.Date(SDOB)) / 365,
    private_attorney = as.numeric(BFSO == 'T'),
    rep = as.factor(BFSO)
  )

issues$issue_type <- as.factor(group_indices(issues, ISSPROG, ISSCODE, ISSLEV1, ISSLEV2, ISSLEV3))

m1 <- glmer(ISSDC == '1' ~ HEARING + (HEARING || issue_type), data = issues, family = binomial, control=glmerControl(optimizer="bobyqa"))
issues$exp <- predict(m1, issues, type = "response")
issues$exp_no_hearing <- predict(m1, data.frame(issue_type = issues$issue_type, HEARING = 0), type = "response")
issues$exp_hearing <- predict(m1, data.frame(issue_type = issues$issue_type, HEARING = 1), type = "response")

obs_hearing_diff <- sum(issues$ISSDC == "1" & issues$HEARING) / sum(issues$HEARING) - sum(issues$ISSDC == "1" & !issues$HEARING) / sum(!issues$HEARING)
median_hearing_diff <- (sum(issues$exp_hearing) - sum(issues$exp_no_hearing)) / nrow(issues)

sum(issues$exp_hearing) / nrow(issues)
sum(issues$exp_no_hearing) / nrow(issues)

m.null <- glmer(ISSDC == '1' ~ 1 | issue_type, data = issues, family = binomial, control=glmerControl(optimizer="bobyqa"))
icc(m.null)

ci.wald <- confint(m1, method="Wald")


set.seed(3317)
b_par <- bootMer(m1, fixef, nsim = 50)
ci.bootstrap <- boot.ci(b_par, type = "perc", index = 2)

hearing.coef <- m1@beta[2]
hearing.ci <- ci.bootstrap$percent[1, 4:5]
hearing.ci_diff <- hearing.ci - hearing.coef

issues %<>%
  mutate(
    exp_hearing.logit = log(exp_hearing / (1 - exp_hearing)),
    exp_hearing.l.logit = exp_hearing.logit + hearing.ci_diff[1],
    exp_hearing.u.logit = exp_hearing.logit + hearing.ci_diff[2],
    exp_hearing.l = exp(exp_hearing.l.logit) / (1 + exp(exp_hearing.l.logit)),
    exp_hearing.u = exp(exp_hearing.u.logit) / (1 + exp(exp_hearing.u.logit))
  )

median_hearing_diff.l <- (sum(issues$exp_hearing.l) - sum(issues$exp_no_hearing)) / nrow(issues)
median_hearing_diff.u <- (sum(issues$exp_hearing.u) - sum(issues$exp_no_hearing)) / nrow(issues)

set.seed(3317)
cv <- crossv_kfold(issues, k = 5) %>%
  mutate(
    models = map(train, ~ glmer(formula(m1), data = ., family = binomial, control=glmerControl(optimizer="bobyqa")))
  ) %>%
  mutate(
    accuracy = map_dbl(models, ~ auc(roc(response = resp_val(.x), predictor = predict(.x, model.frame(.x)))))
  )

cv.null <- crossv_kfold(issues, k = 5) %>%
  mutate(
    models = map(train, ~ glmer(formula(m.null), data = ., family = binomial, control=glmerControl(optimizer="bobyqa")))
  ) %>%
  mutate(
    accuracy = map_dbl(models, ~ auc(roc(response = resp_val(.x), predictor = predict(.x, model.frame(.x)))))
  )

mean(cv$accuracy)
mean(cv.null$accuracy)


issues.by_type <- issues %>%
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
    logit_allowed_rate = log(total_allowed_rate / (1 - total_allowed_rate)),
    logit_remanded_rate = log(total_remanded_rate / (1 - total_remanded_rate))
  )

qplot(data = issues.by_type, x = total_n, y = diff_allowed_rate)
qplot(data = issues.by_type, x = total_allowed_rate, y = diff_allowed_rate, alpha = total_n)
