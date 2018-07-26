source("R/vacolsConnect.R")

library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)
library(ggplot2)

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

dva_theme <- theme_light() +
  theme(
    panel.border = element_blank(),
    text = element_text(family = "Source Sans Pro"),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color = "black"),
    legend.position = "top"
  )

attorneys <- query("
select
  CURRENT_BVA_DUTY_DATE START_DATE,
  STATUS_CHANGE_DATE2 END_DATE

from EMPLOYEE

where ATTORNEY_ID is not null
  and (STATUS_CHANGE_DATE2 is not null or FTE > 0)
  and CURRENT_BVA_DUTY_DATE > date '1900-01-01'
  and (STATUS_CHANGE_DATE2 > date '1900-01-01' or STATUS_CHANGE_DATE2 is null)
") %>%
  transmute(
    start = as_date(START_DATE),
    end = as_date(END_DATE)
  ) %>%
  replace_na(list(end = today() + 1)) %>%
  transmute(
    tenure = interval(start, end),
    probation = interval(start, start + 180)
  )

months <- data.frame(month = seq(date('2010-10-01'), today(), by = 'months')) %>%
  mutate(
    int = interval(month, lead(month))
  )

months <- months[1:(nrow(months) - 1),]

months %<>% mutate(
    fte = sapply(int, function (x) sum(as.numeric(lubridate::intersect(x, attorneys$tenure)), na.rm = TRUE)) / as.numeric(int),
    probation = sapply(int, function (x) sum(as.numeric(lubridate::intersect(x, attorneys$probation)), na.rm = TRUE)) / as.numeric(int)
)

ggplot(months, aes(x = month)) +
  scale_y_continuous(limits = c(0, 800)) +
  geom_area(aes(y = fte), fill = "#256FB2") +
  geom_area(aes(y = probation), fill = "#0F2D52")

decisions <- query("
    select BFDDEC, count(*) DECISIONS
    from BRIEFF
    where BFDC in ('1', '3', '4')
      and BFDDEC >= date '2010-10-01'
    group by BFDDEC
  ") %>%
  mutate(
    month = as_date(paste0(substr(BFDDEC, 1, 7), "-01"))
  ) %>%
  group_by(month) %>%
  summarize(n = sum(DECISIONS))

productivity <- months %>%
  inner_join(decisions, by = c("month")) %>%
  mutate(
    decisions_per_atty = n / fte,
    adj_decisions_per_atty = n / (fte - probation),
    needed = 97907 / 12 / (fte - probation)
  )

productivity_target = 11.5

ggplot(productivity, aes(x = month, y = adj_decisions_per_atty, label = paste(as.character(adj_decisions_per_atty), "decisions"))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 15)) +
  geom_line(aes(y = needed), color = "#9F2F3F", linetype = "dashed", alpha = 0.5) +
  geom_text(data = data.frame(adj_decisions_per_atty = productivity_target, month = max(productivity$month)), color = "#9F2F3F", hjust = .75, vjust = -1, size = 3, family = "Source Sans Pro", fontface = "bold") +
  # geom_hline(yintercept = productivity_target, color = "#9F2F3F", linetype = "dashed", alpha = 0.5) +
  geom_point(pch = 4, color = "#0F2D52") +
  geom_smooth(method = 'loess', span = 0.5, se = FALSE, color = "#0F2D52") +
  # geom_line(color = "#0F2D52") +
  dva_theme

ggsave("impact_reader_productivity.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")
