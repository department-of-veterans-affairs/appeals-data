# How many hearings are held at each RO?

source("R/vacolsConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")
library("ggplot2")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

ro <- read.csv('data/ro.csv', stringsAsFactors = FALSE) %>%
  select(BFREGOFF, RO = Name)

n_hearings <- query("
  select BFREGOFF,
    count(*) NUM_HEARINGS,
    sum(case when HEARING_DISP = 'H' then 1 end) N_HELD,
    sum(case when HEARING_DISP = 'N' then 1 end) N_NOSHOW,
    sum(case when HEARING_DISP = 'P' then 1 end) N_POSTPONED,
    sum(case when HEARING_DISP = 'C' then 1 end) N_CANCELLED

  from HEARSCHED

  join BRIEFF on FOLDER_NR = BFKEY

  where HEARING_TYPE in ('T', 'V')
    and HEARING_DATE >= date '2016-10-01'
    and HEARING_DATE < date '2017-10-01'

  group by BFREGOFF

  order by BFREGOFF
")

n_pending <- query("
select BFCORLID, BFHR, BFDOCIND, BFMPRO, BFREGOFF, BF41STAT
from BRIEFF
where (
    BRIEFF.BFMPRO = 'ADV'
    and BRIEFF.BFHR = '2'
    and (BFHA is null or BFHA = '3')
  ) or (
    BRIEFF.BFMPRO = 'REM'
    and BRIEFF.BFHR = '2'
    and (
      (BFDTB > BFDDEC)
      or vacols.rr_hearing(BFKEY) = 'Y'
    )
    and vacols.hearing_held_postrem(BFKEY, BFDDEC) <> 'Y'
  )
")

n_pending.by_ro <- n_pending %>%
  mutate(
    ready = !is.na(BF41STAT),
    video = !is.na(BFDOCIND) & BFDOCIND == 'V',
    advance = BFMPRO == 'ADV'
  ) %>%
  group_by(BFREGOFF) %>%
  summarize(
    n = n(),
    n_advance = sum(advance),
    n_remand = n - n_advance,
    n_video = sum(video),
    n_tb = n - n_video,
    n_adv_ready = sum(advance & ready)
  )

ro %>%
  inner_join(n_hearings, by = 'BFREGOFF') %>%
  inner_join(n_pending.by_ro, by = 'BFREGOFF') %>%
  write.csv("meredith.csv", row.names = FALSE)
