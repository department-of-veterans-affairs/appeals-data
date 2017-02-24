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
  FOLDER_NR,
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
where FOLDER_NR not like 'VIDEO %'
  and HEARING_TYPE in ('C', 'T', 'V')
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
    sameday_schedule = adv_schedule == 0,
    sameday_exception = ifelse(notheld, MDTIME >= HEARING_DATE, NA)
  ) %>%
  filter(
    adv_schedule >= 0,
    adv_schedule < 365
  )

hist(hearings$adv_schedule, breaks = 365)

hearings.by_advance_date <- hearings %>%
  group_by(adv_schedule, HEARING_TYPE) %>%
  summarize(n_held = sum(held), n_notheld = sum(notheld)) %>%
  mutate(
    n_total = n_held + n_notheld,
    percent_held = n_held / n_total
  )

ggplot(hearings.by_advance_date, aes(x = adv_schedule, y = percent_held, color = HEARING_TYPE, alpha = n_total, weight = n_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~HEARING_TYPE, ncol = 1)

m1 <- glm(held ~ adv_schedule + HEARING_TYPE, family=binomial, data=hearings)
m2 <- glm(held ~ adv_schedule + HEARING_TYPE, family=binomial, data=hearings[hearings$HEARING_TYPE %in% c("T", "V"),])
m3 <- glm(held ~ adv_schedule, family=binomial, data=hearings[hearings$HEARING_TYPE %in% c("C"),])
m4 <- glm(noshow ~ adv_schedule + HEARING_TYPE, family=binomial, data=hearings[hearings$HEARING_TYPE %in% c("T", "V"),])
m5 <- glm(noshow ~ adv_schedule, family=binomial, data=hearings[hearings$HEARING_TYPE %in% c("C"),])

newdata <- data.frame(adv_schedule = rep(c(0, 30, 60, 90), 2), HEARING_TYPE = rep(c("T", "V"), each = 4))
newdata.predict <- predict(m2, newdata, type = "response", se.fit = TRUE)
newdata$predict <- newdata.predict$fit
newdata$ci <- newdata.predict$se.fit * 1.96

