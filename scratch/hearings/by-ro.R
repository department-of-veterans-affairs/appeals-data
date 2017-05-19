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
    count(*) NUM_HEARINGS

  from HEARSCHED

  join BRIEFF on FOLDER_NR = BFKEY

  where HEARING_TYPE in ('T', 'V')
    and HEARING_DISP = 'H'
    and HEARING_DATE >= date '2015-10-01'
    and HEARING_DATE < date '2016-10-01'

  group by BFREGOFF
") %>%
  left_join(ro, by = c('BFREGOFF')) %>%
  arrange(desc(NUM_HEARINGS))
