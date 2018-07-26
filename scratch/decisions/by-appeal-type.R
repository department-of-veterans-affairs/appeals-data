source("R/vacolsConnect.R")

library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(scales)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

decisions <- query("
select
  BFKEY,
  BFDDEC,
  (case when VACOLS.AOD_CNT(BFKEY) > 0 then 1 else 0 end) as AOD,
  BFAC

from BRIEFF

where BFAC in ('1', '3')
  and BFDC between '1' and '9'
  and BFDDEC >= date '2004-10-01'
  and BFDDEC < date '2018-06-01'
") %>% mutate(
  AOD = as.logical(AOD),
  post_rem = BFAC == '3',
  type = factor(ifelse(post_rem, "postrem", ifelse(AOD, "orig_aod", "orig_nonaod")), levels = c('orig_nonaod', 'orig_aod', 'postrem')),
  BFDDEC = as.Date(BFDDEC),
  orig_aod = type == "orig_aod",
  orig_nonaod = type == "orig_nonaod"
)

decisions.by_month <- decisions %>%
  mutate(
    month = as.Date(paste0(substr(BFDDEC, 1, 7), "-01"))
  ) %>%
  group_by(month) %>%
  summarise(
    postrem = sum(post_rem),
    orig_aod = sum(orig_aod),
    orig_nonaod = sum(orig_nonaod),
    total = n()
  ) %>%
  gather(type, cnt, -month, -total) %>%
  mutate(type = factor(type, levels = rev(c('orig_nonaod', 'orig_aod', 'postrem')), ordered = TRUE))

dva_theme <- theme_light() +
  theme(
    panel.border = element_blank(),
    text = element_text(family = "Source Sans Pro"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color = "black")
  )

ggplot(decisions.by_month, aes(x = month, y = cnt / total, color = type)) +
  geom_point(pch = 4) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = rev(c("#0F2D52", "#256FB2", "#9F2F3F"))) +
  dva_theme

aod.by_month <- decisions %>%
  mutate(
    month = as.Date(paste0(substr(BFDDEC, 1, 7), "-01"))
  ) %>%
  group_by(month) %>%
  summarise(
    aod = sum(AOD),
    nonaod = n() - sum(AOD),
    total = n()
  ) %>%
  gather(type, cnt, -month, -total) %>%
  mutate(type = factor(type, levels = rev(c('nonaod', 'aod')), ordered = TRUE))

ggplot(aod.by_month, aes(x = month, y = cnt / total, color = type)) +
  geom_point(pch = 4) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = rev(c("#0F2D52", "#256FB2", "#9F2F3F"))) +
  dva_theme
