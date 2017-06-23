source("R/vacolsConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")
library("edeaR")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

appeals <- query("
select
  BFKEY,
  BFDDEC,
  BFDPDCN,
  BFDNOD,
  BFDSOC,
  BFD19,
  BF41STAT,
  BFDC,
  BFSSOC1,
  BFSSOC2,
  BFSSOC3,
  BFSSOC4,
  BFSSOC5,
  TIDKTIME

from BRIEFF

join FOLDER on BFKEY = TICKNUM

where (BFDC is null and BFMPRO <> 'HIS') or BFDDEC >= date '2016-06-21'
  and BFAC in ('1', '3', '7')
  and BFDNOD >= date '2006-10-01'
")

hearing_events <- data.frame(HEARING_DISP = c('H', 'C', 'N'), event = c('hearing_held', 'hearing_cancelled', 'hearing_no_show'), stringsAsFactors = FALSE)

hearings <- query("
select
  BFKEY,
  HEARING_DATE,
  HEARING_DISP

from BRIEFF

join HEARSCHED on BFKEY = FOLDER_NR

where ((BFDC is null and BFMPRO <> 'HIS') or BFDDEC >= date '2017-06-21')
  and BFAC in ('1', '3', '7')
  and HEARING_TYPE in ('C', 'T', 'V')
  and HEARING_DISP in ('H', 'C', 'N')
  and BFDPDCN is null or HEARING_DATE > BFDPDCN
  and BFDNOD >= date '2006-10-01'
") %>%
  inner_join(hearing_events, by = 'HEARING_DISP') %>%
  transmute(
    appeal = BFKEY,
    date = as.Date(HEARING_DATE),
    event
  )

cavc <- query("
select
  BFKEY,
  CVDDEC

from BRIEFF

join COVA on BFKEY = CVFOLDER

where (BFDC is null and BFMPRO <> 'HIS') or BFDDEC >= date '2017-06-21'
  and BFAC in ('1', '3', '7')
  and BFDPDCN is null or CVDDEC > BFDPDCN
  and BFDNOD >= date '2006-10-01'
") %>%
  transmute(
    appeal = BFKEY,
    date = as.Date(CVDDEC),
    event = 'cavc_decision'
  ) %>%
  unique()

dispositions <- data.frame(
  BFDC  = c('1',                  '3',          '4',                  '5',                  '6',                  '8',                  '9',         'A',           'B',           'D',     'E',         'F',         'G',         'L',          'M',      'W',         'X'),
  event = c('bva_final_decision', 'bva_remand', 'bva_final_decision', 'bva_final_decision', 'bva_final_decision', 'bva_final_decision', 'withdrawn', 'field_grant', 'field_grant', 'other', 'withdrawn', 'withdrawn', 'withdrawn', 'bva_remand', 'merged', 'withdrawn', 'withdrawn'),
  stringsAsFactors = FALSE
)

event_types <- data.frame(
  column = c('BFDNOD', 'BFDSOC', 'BFD19', 'BF41STAT', 'BFSSOC1', 'BFSSOC2', 'BFSSOC3', 'BFSSOC4', 'BFSSOC5', 'TIDKTIME'),
  event  = c('nod', 'soc', 'form_9', 'certification', 'ssoc', 'ssoc', 'ssoc', 'ssoc', 'ssoc', 'activation'),
  stringsAsFactors = FALSE
)

events <- appeals %>%
  gather(column, date, -BFKEY, -BFDC, -BFDPDCN) %>%
  filter(!is.na(date), or(is.na(BFDPDCN), date >= BFDPDCN)) %>%
  left_join(dispositions, by = 'BFDC') %>%
  left_join(event_types, by = 'column') %>%
  transmute(
    appeal = BFKEY,
    date = as.Date(date),
    event = ifelse(column == 'BFDDEC', event.x, event.y)
  ) %>%
  rbind(hearings) %>%
  rbind(cavc) %>%
  arrange(appeal, date)

log <- events %>%
  mutate(instId = row_number()) %>%
  eventlog(case_id = "appeal",
           activity_id = "event",
           activity_instance_id = "instId",
           lifecycle_id = "appeal",
           timestamp = "date")

traces <- trace_coverage(log, level_of_analysis = "trace")

write.csv(traces[1:88,c(1, 2, 4)], "appeals-status-traces.csv", row.names = FALSE)
