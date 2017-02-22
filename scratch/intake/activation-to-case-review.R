## How long does it take between activation and case review, and how has that changed over time?

source("R/vacolsConnect.R")
library(dplyr)
library(ggplot2)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

activation.control <- query("select BFKEY, BF41STAT, TIDRECV, TIDKTIME from BRIEFF join FOLDER on BFKEY = TICKNUM where BFAC = '1' and BFHR = '5' and TIVBMS = 'Y' and TIDKTIME >= date '2016-01-01' and TIDKTIME < date '2016-04-01' and BFDCERTOOL is null")
activation.caseflow <- query("select BFKEY, BF41STAT, TIDRECV, TIDKTIME from BRIEFF join FOLDER on BFKEY = TICKNUM where BFAC = '1' and BFHR = '5' and TIVBMS = 'Y' and TIDKTIME >= date '2016-10-01' and TIDKTIME < date '2017-01-01' and BFDCERTOOL is not null")

activation.control %<>%
  mutate(
    receive_time = as.numeric(as.Date(TIDRECV) - as.Date(BF41STAT)),
    docket_time = as.numeric(as.Date(TIDKTIME) - as.Date(TIDRECV))
  ) %>%
  filter(docket_time >= 0, docket_time < 200)

activation.caseflow %<>%
  mutate(
    receive_time = as.numeric(as.Date(TIDRECV) - as.Date(BF41STAT)),
    docket_time = as.numeric(as.Date(TIDKTIME) - as.Date(TIDRECV))
  )

summary(activation.control$docket_time)
summary(activation.caseflow$docket_time)
hist(activation.control$docket_time)

activation <- query("select BFKEY, BF41STAT, BFDCERTOOL, TIDRECV, TIDKTIME from BRIEFF join FOLDER on BFKEY = TICKNUM where BFAC = '1' and BFHR = '5' and TIVBMS = 'Y' and TIDKTIME >= date '2015-01-01' and TIDKTIME < date '2017-01-01'") %>%
  mutate(
    receive_time = as.numeric(as.Date(TIDRECV) - as.Date(BF41STAT)),
    review_time = as.numeric(as.Date(TIDKTIME) - as.Date(TIDRECV)),
    receive_date = as.Date(TIDRECV),
    review_date = as.Date(TIDKTIME),
    caseflow = !is.na(BFDCERTOOL)
  ) %>%
  filter(review_time >= 0, review_time < 365)

ggplot(activation, aes(x = review_date, y = review_time, color = caseflow)) +
  scale_x_date(limits = range(activation$review_date)) +
  geom_point(alpha = 0.25)

ggplot(activation, aes(x = receive_date, fill = caseflow)) +
  scale_x_date(limits = range(activation$review_date)) +
  geom_histogram(binwidth = 1)
