# What happens to non-VHA cases at Dispatch? https://github.com/department-of-veterans-affairs/appeals-pm/issues/1112

source("R/vacolsConnect.R")
source("R/events.R")
library(dplyr)

con <- vacolsConnect()

crit <- "BFDC in ('1', '3') and BFDDEC >= date '2015-10-01' and BFDDEC < date '2016-10-01' and vacols.issue_cnt_remand(BFKEY) > 0"

remand_events <- event_getPriorLocs(con, crit)
remand_folder <- query(paste("select BFKEY, BFREGOFF, BFSO from BRIEFF where", crit))

ro <- read.csv("data/ro.csv", stringsAsFactors = FALSE) %>%
  select(BFREGOFF, Category) %>%
  mutate(Category = ifelse(Category == "", "Generic", Category))

remands <- remand_events %>%
  group_by(BFKEY) %>%
  mutate(next_LOC = lead(LOC)) %>%
  filter(LOC == '30', next_LOC %in% c("50", "51", "52", "53", "54", "70", "97")) %>%
  filter(row_number() == n()) %>%
  inner_join(remand_folder, by = c("BFKEY")) %>%
  inner_join(ro, by = c("BFREGOFF"))

not_private_attorney_remands <- remands %>%
  filter(BFSO != "T")

table(remands$next_LOC, remands$Category)
table(not_private_attorney_remands$next_LOC, not_private_attorney_remands$Category)
