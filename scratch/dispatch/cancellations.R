# Cancellation rate over time

source("R/caseflowConnect.R")

library("dplyr")
library("ggplot2")

cfCon <- caseflowConnect()
cfQuery <- function (query) { return(dbGetQuery(cfCon, query)) }

test <- cfQuery("select completed_at, completion_status from tasks where completion_status <> 2 and completed_at is not null") %>%
  transmute(completed_at = as.Date(completed_at), cancelled = completion_status == '1') %>%
  group_by(completed_at) %>%
  summarize(n = n(), cancellation_rate = sum(cancelled) / n())

ggplot(test, aes(x = completed_at, y = cancellation_rate, alpha = n, weight = n)) +
  guides(alpha = FALSE) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

cancellations <- cfQuery("select completed_at, completion_status, comment from tasks where completion_status = 1 and completed_at is not null")
