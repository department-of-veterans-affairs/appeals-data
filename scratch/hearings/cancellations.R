## How far in advance of the scheduled hearing date is it cancelled?
## Do hearings scheduled further in the future have a higher cancellation rate?
## Do cancellation rates vary by RO?
## How often are hearings added on the same day as they occur?
## Of docket days with cancelled hearings, how often is another hearing added to the docket?

source("R/vacolsConnect.R")
library(dplyr)
library(ggplot2)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

hearings <- query("
select
  BFKEY,
  BFREGOFF,
  BFSO,
  HEARING_PKSEQ,
  HEARING_DATE,
  HEARING_TYPE,
  HEARING_DISP,
  CANCELDATE,
  ADDTIME,
  ADDUSER,
  MDTIME,
  MDUSER
from HEARSCHED
inner join BRIEFF on FOLDER_NR = BFKEY
where HEARING_TYPE in ('C', 'T', 'V')
  and HEARING_DISP is not null
  and HEARING_DATE >= date '2015-10-01'
  and HEARING_DATE < date '2016-10-01'
  and ADDTIME >= date '2010-10-01'
  and MDTIME >= date '2010-10-01'
") %>%
  mutate(
    HEARING_DATE = as.Date(HEARING_DATE),
    CANCELDATE = as.Date(CANCELDATE),
    ADDTIME = as.Date(ADDTIME),
    MDTIME = as.Date(MDTIME),
    cancelled = HEARING_DISP %in% c("C", "P"),
    noshow = HEARING_DISP == "N",
    held = HEARING_DISP == "H",
    notheld = HEARING_DISP != "H",
    adv_schedule = as.numeric(HEARING_DATE - ADDTIME),
    scheduled_same_day = adv_schedule == 0,
    scheduled_within_30 = adv_schedule <= 30
  ) %>%
  filter(
    adv_schedule >= 0,
    adv_schedule < 365
  )

hist(hearings$adv_schedule, breaks = 365)

hearings_at_ro <- hearings[hearings$HEARING_TYPE %in% c("T", "V"),]
hearings_at_co <- hearings[hearings$HEARING_TYPE == "C",]

m1 <- glm(noshow ~ scheduled_within_30, family=binomial, data=hearings_at_ro)
summary(m1)
predict(m1, data.frame(scheduled_within_30 = c(TRUE, FALSE)), type = "response", se.fit = TRUE)


m2 <- glm(noshow ~ scheduled_within_30, family=binomial, data=subset(hearings_at_ro, !cancelled))
m3 <- glm(noshow ~ adv_schedule, family=binomial, data=subset(hearings_at_ro, scheduled_within_30))
predict(m3, data.frame(adv_schedule = c(0, 7, 14, 28)), type = "response", se.fit = TRUE)

m4 <- glm(noshow ~ adv_schedule, family = binomial, data=subset(hearings_at_ro, adv_schedule > 30 & adv_schedule <= 90))
predict(m4, data.frame(adv_schedule = c(30, 60, 90)), type = "response", se.fit = TRUE)






m2 <- glm(noshow ~ BFREGOFF, family=binomial, data=hearings_at_ro)
m3 <- glm(noshow ~ BFREGOFF, family=binomial, data=subset(hearings_at_ro, BFREGOFF %in% c("RO13", "RO06", "RO25")))

newdata <- data.frame(BFREGOFF = unique(hearings_at_ro$BFREGOFF))
newdata.predict <- predict(m2, newdata, type = "response", se.fit = TRUE)
newdata$predict <- newdata.predict$fit
newdata$ci <- newdata.predict$se.fit * 1.96

ggplot(subset(newdata, predict < .5 & (predict - ci > sum(hearings$noshow) / nrow(hearings) | predict + ci < sum(hearings$noshow) / nrow(hearings)) & predict > 0.001), aes(x = reorder(BFREGOFF, desc(predict)), y = predict, ymin = pmax(predict - ci, 0), ymax = predict + ci)) +
  geom_point() +
  geom_linerange() +
  geom_hline(yintercept = sum(hearings$noshow) / nrow(hearings))





m1 <- glm(held ~ adv_schedule + HEARING_TYPE, family=binomial, data=hearings)
m2 <- glm(held ~ adv_schedule + HEARING_TYPE, family=binomial, data=hearings[hearings$HEARING_TYPE %in% c("T", "V"),])
m3 <- glm(held ~ adv_schedule, family=binomial, data=hearings[hearings$HEARING_TYPE %in% c("C"),])
m4 <- glm(sameday_exception ~ month_or_less, family=binomial, data=hearings[hearings$HEARING_TYPE %in% c("T", "V"),])
m5 <- glm(noshow ~ adv_schedule, family=binomial, data=hearings[hearings$HEARING_TYPE %in% c("C"),])

newdata <- data.frame(adv_schedule = rep(c(0, 30, 60, 90), 2), HEARING_TYPE = rep(c("T", "V"), each = 4))
newdata.predict <- predict(m2, newdata, type = "response", se.fit = TRUE)
newdata$predict <- newdata.predict$fit
newdata$ci <- newdata.predict$se.fit * 1.96

hearings.by_advance_date <- hearings %>%
  group_by(adv_schedule, HEARING_TYPE) %>%
  summarize(n_held = sum(held), n_notheld = sum(notheld), n_noshow = sum(noshow)) %>%
  mutate(
    n_total = n_held + n_notheld,
    percent_held = n_held / n_total,
    percent_noshow = n_noshow / n_total
  )

ggplot(subset(hearings.by_advance_date, adv_schedule > 30 & adv_schedule <= 90 & HEARING_TYPE != "C"), aes(x = adv_schedule, y = percent_noshow, color = HEARING_TYPE, alpha = n_total, weight = n_total)) +
  geom_point() +
  geom_smooth(method = "loess", span = 1) +
  facet_wrap(~HEARING_TYPE, ncol = 1)

