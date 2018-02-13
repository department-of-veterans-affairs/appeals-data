source("R/vacolsConnect.R")
source("R/caseflowConnect.R")

library("dplyr")
library("magrittr")
library("tidyr")
library("ggplot2")
library("scales")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

cfcon <- caseflowConnect()
cfquery <- function (query) { return(dbGetQuery(cfcon, query)) }

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


##########################
# Caseflow Certification #
##########################

certifications <- dbGetQuery(con, "
  select BF41STAT, BFDCERTOOL
  from BRIEFF
  where BF41STAT >= date '2016-01-01'
") %>%
  transmute(
    caseflow = !is.na(BFDCERTOOL),
    month = substr(BF41STAT, 1, 7)
  ) %>%
  count(caseflow, month) %>%
  group_by(month) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  filter(caseflow) %>%
  mutate(rate = n / total, last = n() == row_number(), date = as.Date(paste0(month, "-01")))

certifications.paperless <- dbGetQuery(con, "
  select BF41STAT, BFDCERTOOL
  from BRIEFF join FOLDER on BFKEY = TICKNUM
  where BF41STAT >= date '2016-01-01' and TIVBMS = 'Y'
") %>%
  transmute(
    caseflow = !is.na(BFDCERTOOL),
    month = substr(BF41STAT, 1, 7)
  ) %>%
  count(caseflow, month) %>%
  group_by(month) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  filter(caseflow) %>%
  mutate(rate = n / total, last = n() == row_number(), date = as.Date(paste0(month, "-01")))

certifications <- certifications[1:(nrow(certifications) - 1),]
certifications.paperless <- certifications.paperless[1:(nrow(certifications.paperless) - 1),]

ggplot(certifications, aes(x = date, y = rate, group = 1, label = percent(rate))) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  geom_line(color = "#0F2D52", size = 1) +
  geom_text(data = certifications[nrow(certifications),], vjust = -.6, color = "#0F2D52", fontface = "bold", size = 3, family = "Source Sans Pro") +
  geom_point(data = certifications[nrow(certifications),], color = "#0F2D52") +
  dva_theme

ggsave("all_certifications.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")

ggplot(certifications.paperless, aes(x = date, y = rate, group = 1, label = percent(rate))) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  geom_line(color = "#0F2D52", size = 1) +
  geom_text(data = certifications.paperless[nrow(certifications.paperless),], vjust = -.6, color = "#0F2D52", fontface = "bold", size = 3, family = "Source Sans Pro") +
  geom_point(data = certifications.paperless[nrow(certifications.paperless),], color = "#0F2D52") +
  dva_theme

ggsave("vbms_certifications.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")

advance <- query("select BFKEY, BF41STAT, BFDCERTOOL from BRIEFF where BFMPRO = 'ADV' and BF41STAT is not null")
n_advance <- nrow(advance)
n_cf_advance <- sum(!is.na(advance$BFDCERTOOL))

mismatch_report_1 <- read.csv('../caseflow-certification/reports/mismatch_2018-01-10.csv')
mismatch_report_2 <- read.csv('../caseflow-certification/reports/mismatch_2018-01-11.csv')

n_mismatch <- rbind(mismatch_report_1, mismatch_report_2)$BFKEY %>% unique() %>% length()
n_cf_mismatch <- (rbind(mismatch_report_1, mismatch_report_2) %>% filter(CASEFLOW == 'Y'))$BFKEY %>% unique() %>% length()

n_mismatch <- nrow(mismatch_report)
n_cf_mismatch <- sum(mismatch_report$CASEFLOW == 'Y')

mismatch_rate <- n_mismatch / n_advance
cf_mismatch_rate <- n_cf_mismatch / n_cf_advance

mismatches <- data.frame(
  month = as.Date(c("2015-12-01","2016-10-01","2017-01-01","2017-04-01","2017-07-01","2018-01-01")),
  n = c(0.406, 0.354, 0.282, 0.244, 0.191, 0.115)
)

ggplot(mismatches, aes(x = month, y = n, label = percent(n))) +
  scale_x_date(breaks = mismatches$month, date_labels = "%b %Y") +
  scale_y_continuous(label=percent) +
  geom_bar(fill = "#0F2D52", stat = "identity") +
  geom_text(nudge_y = 0.02, fontface = "bold", size = 3) +
  geom_hline(yintercept = mismatch_target, color = "#9F2F3F", linetype = "dashed", alpha = 0.5) +
  geom_text(data = data.frame(n = mismatch_target, month = max(mismatches$month) + 90), color = "#9F2F3F", hjust = 0, vjust = -.33, size = 3, family = "Source Sans Pro", fontface = "bold") +
  dva_theme

ggsave("impact_certification_mismatch.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")


activations <- query("
  select BF41STAT, TIDRECV, (case when BFHR in ('1', '2') then 1 else 0 end) HEARING
  from BRIEFF
  inner join FOLDER on BFKEY = TICKNUM
  where BFAC = '1'
    and TIVBMS = 'Y'
    and BF41STAT is not null
    and TIDRECV >= date '2015-10-01'
")

activations.by_month <- activations %>%
  transmute(
    time_to_activation = as.numeric(as.Date(TIDRECV) - as.Date(BF41STAT)),
    month = as.Date(paste0(substr(TIDRECV, 1, 7), "-01")),
    hearing = as.logical(HEARING)
  ) %>%
  filter(time_to_activation >= 0) %>%
  group_by(month, hearing) %>%
  summarise(adp = mean(time_to_activation)) %>%
  filter(month < today() - months(1))

activations.non_hearing <- subset(activations.by_month, hearing == FALSE)

activation_target = 60

ggplot(activations.non_hearing, aes(x = month, y = adp, label = paste(as.character(adp), "days"))) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 400)) +
  geom_text(data = data.frame(adp = activation_target, month = max(activations.non_hearing$month) + 60), color = "#9F2F3F", hjust = .55, vjust = -.33, size = 3, family = "Source Sans Pro", fontface = "bold") +
  geom_hline(yintercept = activation_target, color = "#9F2F3F", linetype = "dashed", alpha = 0.5) +
  geom_smooth(method = 'loess', span = 2, se = FALSE, color = "#0F2D52") +
  geom_point(pch = 4, color = "#0F2D52") +
  dva_theme

ggsave("impact_certification_activation.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")

#####################
# Caseflow Dispatch #
#####################

decisions <- query("
  select
    BFKEY,
    BFCORLID,
    BFDDEC,
    TIOCTIME

  from BRIEFF

  inner join FOLDER on BFKEY = TICKNUM

  left join (
    select count(*) as ACT_ISSUES,
      ISSKEY

    from ISSUES

    where ISSDC = '3' or
      (ISSDC = '1' and not (ISSPROG = '02' and ISSCODE = '15' and ISSLEV1 = '04'))

    group by ISSKEY
  ) on BFKEY = ISSKEY

  where BFDDEC is not null
    and BFDC between '1' and '9'
    and ACT_ISSUES > 0
    and TIVBMS = 'Y'
    and TIOCTIME >= date '2016-04-01' and TIOCTIME < date '2018-02-01'
")

vets <- unique(decisions$BFCORLID)
write.table(vets, "sensitive_data/recent_ids_for_claims.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

claims <- read.csv("sensitive_data/claims_17-12.csv", stringsAsFactors = FALSE) %>% unique()

decisions %<>%
  group_by(BFCORLID, BFDDEC) %>%
  mutate(
    case_decision_date = as.Date(BFDDEC),
    outcoding_date = as.Date(TIOCTIME),
    multi_decision = n() > 1
  ) %>%
  ungroup()

merged_decisions_claims <- decisions %>%
  left_join(claims, by = c("BFCORLID")) %>%
  mutate(
    claim_decision_date = as.Date(claim_receive_date, "%m/%d/%Y"),
    journal_date = as.Date(journal_date, "%m/%d/%Y"),
    precise_match = case_decision_date == claim_decision_date,
    fuzzy_match = !precise_match & (abs(case_decision_date - claim_decision_date) <= 31 | abs(outcoding_date - claim_decision_date) <= 31)
  )

matches <- merged_decisions_claims %>%
  filter(precise_match | fuzzy_match) %>%
  select(BFCORLID, BFDDEC, precise_match, fuzzy_match) %>%
  group_by(BFCORLID, BFDDEC) %>%
  summarise(has_match = TRUE, has_precise_match = sum(precise_match) > 0)

merged_decisions_claims %<>%
  left_join(matches, by = c("BFCORLID", "BFDDEC"))

merged_decisions_claims$journal_date[merged_decisions_claims$journal_date < merged_decisions_claims$outcoding_date] <- NA

merged_decisions_claims %<>%
  group_by(BFCORLID, BFDDEC) %>%
  slice(which.min(ifelse(is.na(journal_date), Inf, journal_date))) %>%
  ungroup() %>%
  select(BFCORLID, BFDDEC, has_match, has_precise_match, benefit_claim_id, claim_decision_date, journal_date, journal_object_id, journal_station, journal_user_id, claim_type_code, claim_type_name, end_product_type_code, status_type_code)

decisions %<>%
  left_join(merged_decisions_claims, by = c("BFCORLID", "BFDDEC")) %>%
  mutate(
    time_to_ep = journal_date - outcoding_date,
    amc_ep = journal_station == 397
  )

clean_decisions <- decisions %>%
  filter(!multi_decision) %>%
  replace_na(list(has_match = FALSE, has_precise_match = FALSE))

# FOR ITERATIVELY FETCHING ADDITIONAL CLAIMS
missing <- clean_decisions %>% filter(!has_match)
missing_vets <- unique(missing$BFCORLID)
write.table(missing_vets, "sensitive_data/recent_ids_for_claims.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

by_month <- clean_decisions %>%
  mutate(
    month = as.Date(paste0(substr(TIOCTIME, 1, 7), "-01"))
  ) %>%
  group_by(month) %>%
  summarize(
    total = n(),
    matched = sum(has_match),
    precise_matched = sum(has_precise_match),
    within_a_week = sum(time_to_ep <= 7, na.rm = TRUE),
    median_time_to_ep = median(time_to_ep, na.rm = TRUE),
    max_time_to_ep = max(time_to_ep, na.rm = TRUE),
    x75_time_to_ep = quantile(time_to_ep, probs = 0.75, na.rm = TRUE),
    x95_time_to_ep = quantile(time_to_ep, probs = 0.95, na.rm = TRUE),
    adj_median_time_to_ep = median(c(time_to_ep, rep(Inf, times = sum(is.na(time_to_ep)))), na.rm = TRUE),
    adj_x75_time_to_ep = quantile(c(time_to_ep, rep(Inf, times = sum(is.na(time_to_ep)))), probs = 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    percent_matched = ifelse(row_number() <= n() - 3, matched / total, NA),
    percent_within_a_week = within_a_week / total
  )

by_day <- clean_decisions %>%
  filter(TIOCTIME > as.POSIXct('2017-03-31')) %>%
  group_by(TIOCTIME) %>%
  summarize(
    total = n(),
    matched = sum(has_match),
    precise_matched = sum(has_precise_match),
    within_a_week = sum(time_to_ep <= 7, na.rm = TRUE),
    median_time_to_ep = median(time_to_ep, na.rm = TRUE),
    x75_time_to_ep = quantile(time_to_ep, probs = 0.75, na.rm = TRUE),
    x95_time_to_ep = quantile(time_to_ep, probs = 0.95, na.rm = TRUE),
    adj_median_time_to_ep = median(c(time_to_ep, rep(Inf, times = sum(is.na(time_to_ep)))), na.rm = TRUE),
    adj_x75_time_to_ep = quantile(c(time_to_ep, rep(Inf, times = sum(is.na(time_to_ep)))), probs = 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    percent_matched = ifelse(row_number() <= n() - 3, matched / total, NA),
    percent_within_a_week = within_a_week / total
  )

caseflowCested <- cfquery("select vacols_id from tasks join appeals on appeal_id = appeals.id where type = 'EstablishClaim' and completion_status in ('0', '3')") %>%
  mutate(caseflow = TRUE)

usage_data <- clean_decisions %>%
  mutate(
    month = as.Date(paste0(substr(TIOCTIME, 1, 7), "-01"))
  ) %>%
  select(month, BFKEY) %>%
  left_join(caseflowCested, by = c("BFKEY" = "vacols_id")) %>%
  replace_na(list(caseflow = FALSE)) %>%
  group_by(month) %>%
  summarize(
    total = n(),
    caseflow = sum(caseflow)
  ) %>%
  ungroup() %>%
  mutate(rate = caseflow / total, last = n() == row_number()) %>%
  filter(month >= as.Date('2017-01-01'))

timing_data <- by_month %>%
  select(month, adj_median_time_to_ep, adj_x75_time_to_ep) %>%
  gather(key, value, adj_median_time_to_ep, adj_x75_time_to_ep) %>%
  mutate(key = ifelse(key == 'adj_median_time_to_ep', 'Median', '75th Percentile'))

matched_data <- by_month %>%
  select(month, percent_matched, percent_within_a_week) %>%
  gather(key, value, percent_matched, percent_within_a_week) %>%
  filter(complete.cases(.)) %>%
  mutate(key = ifelse(key == 'percent_matched', 'All Matching EPs', 'EPs Created within 7 Days')) %>%
  group_by(key) %>%
  mutate(last = n() == row_number()) %>%
  ungroup()

timing_targets = c(4)

ggplot(timing_data, aes(x = month, y = value, color = key, label = paste(as.character(value), "days"))) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 30)) +
  scale_color_manual(values = c("#0F2D52", "#256FB2")) +
  geom_hline(yintercept = timing_targets, color = "#9F2F3F", linetype = "dashed", alpha = 0.5) +
  geom_text(data = data.frame(value = timing_targets, month = max(timing_data$month) + 30), color = "#9F2F3F", hjust = .3, vjust = -.33, size = 3, family = "Source Sans Pro", fontface = "bold") +
  # geom_line(method = 'loess', span = 2, se = FALSE) +
  # geom_point(pch = 4) +
  geom_line(size = 1) +
  guides(color = guide_legend(title = '')) +
  dva_theme

ggsave("impact_dispatch_timing.pdf", device = cairo_pdf, width = 5.5, height = 2.3, units = "in")

matched_targets = c(.85, .95)

ggplot(matched_data, aes(x = month, y = value, color = key, label = percent(value))) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  scale_color_manual(values = c("#256FB2", "#0F2D52")) +
  geom_hline(yintercept = matched_targets, color = "#9F2F3F", linetype = "dashed", alpha = 0.5) +
  geom_text(data = data.frame(value = matched_targets, month = max(matched_data$month) + 30), color = "#9F2F3F", hjust = 0, vjust = -.33, size = 3, family = "Source Sans Pro", fontface = "bold") +
  geom_line(size = 1) +
  geom_point(data = subset(matched_data, last), size = 2) +
  geom_text(data = subset(matched_data, last), vjust = -.6, size = 3, family = "Source Sans Pro", fontface = "bold", show.legend = FALSE) +
  guides(color = guide_legend(title = '')) +
  dva_theme

ggsave("impact_dispatch_matched.pdf", device = cairo_pdf, width = 5.5, height = 2.3, units = "in")

ggplot(usage_data, aes(x = month, y = rate, group = 1, label = percent(rate))) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  geom_line(color = "#0F2D52", size = 1) +
  geom_point(data = subset(usage_data, last), color = "#0F2D52", size = 2) +
  geom_text(data = subset(usage_data, last), vjust = 2, size = 3, family = "Source Sans Pro", fontface = "bold", show.legend = FALSE) +
  dva_theme

ggsave("impact_dispatch_usage.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")


###################
# eFolder Express #
###################

users <- data.frame(
  month = c(as.Date("2016-08-01"), as.Date("2016-09-01"), as.Date("2016-10-01"), as.Date("2016-11-01"), as.Date("2016-12-01"), as.Date("2017-01-01"), as.Date("2017-02-01"), as.Date("2017-03-01"), as.Date("2017-04-01"), as.Date("2017-05-01"), as.Date("2017-06-01"), as.Date("2017-07-01"), as.Date("2017-08-01"), as.Date("2017-09-01"), as.Date("2017-10-01"), as.Date("2017-11-01"), as.Date("2017-12-01")),
  n = c(6, 6, 9, 49, 97, 120, 115, 130, 123, 124, 132, 128, 135, 151, 141, 156, 150)
)

users$last <- c(rep(FALSE, nrow(users) - 1), TRUE)

folders <- data.frame(
  month = c(as.Date("2016-08-01"), as.Date("2016-09-01"), as.Date("2016-10-01"), as.Date("2016-11-01"), as.Date("2016-12-01"), as.Date("2017-01-01"), as.Date("2017-02-01"), as.Date("2017-03-01"), as.Date("2017-04-01"), as.Date("2017-05-01"), as.Date("2017-06-01"), as.Date("2017-07-01"), as.Date("2017-08-01"), as.Date("2017-09-01"), as.Date("2017-10-01"), as.Date("2017-11-01"), as.Date("2017-12-01")),
  n = c(253, 60, 24, 348, 1726, 2795, 3519, 4204, 4315, 4471, 4902, 4574, 5185, 4962, 5475, 5869, 5772)
)

folders$last <- c(rep(FALSE, nrow(folders) - 1), TRUE)

documents <- data.frame(
  month = c(as.Date("2016-08-01"), as.Date("2016-09-01"), as.Date("2016-10-01"), as.Date("2016-11-01"), as.Date("2016-12-01"), as.Date("2017-01-01"), as.Date("2017-02-01"), as.Date("2017-03-01"), as.Date("2017-04-01"), as.Date("2017-05-01"), as.Date("2017-06-01"), as.Date("2017-07-01"), as.Date("2017-08-01"), as.Date("2017-09-01"), as.Date("2017-10-01"), as.Date("2017-11-01"), as.Date("2017-12-01")),
  n = c(125325, 17229, 10224, 76266, 411735, 534069, 672929, 775061, 802036, 853097, 905771, 830900, 919102, 1097925, 1600926, 5890557, 5672469)
)

documents$last <- c(rep(FALSE, nrow(documents) - 1), TRUE)

ggplot(users, aes(x = month, y = n, label = n)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 175)) +
  geom_bar(fill = "#0F2D52", stat = "identity") +
  geom_text(data = subset(users, last), vjust = -.6, size = 3, family = "Source Sans Pro", fontface = "bold") +
  dva_theme

ggsave("efolder_users.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")

ggplot(folders, aes(x = month, y = n, label = comma(n))) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(label=comma, limits = c(0, 6000)) +
  geom_bar(fill = "#0F2D52", stat = "identity") +
  geom_text(data = subset(folders, last), vjust = -.6, size = 3, family = "Source Sans Pro", fontface = "bold") +
  dva_theme

ggsave("efolder_folders.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")

ggplot(documents, aes(x = month, y = n, label = comma(n))) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(label=comma, limits = c(0, 6e6)) +
  geom_bar(fill = "#0F2D52", stat = "identity") +
  geom_text(data = subset(documents, last), vjust = -.6, size = 3, family = "Source Sans Pro", fontface = "bold") +
  dva_theme

ggsave("efolder_documents.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")

###################
# Caseflow Reader #
###################

decisions <- query("select BFKEY, BFDDEC from BRIEFF where BFDC between '1' and '9' and BFDDEC >= date '2017-07-01'")
views <- cfquery("select distinct(VACOLS_ID) from APPEAL_VIEWS join APPEALS on APPEAL_ID = APPEALS.ID")

by_month <- views %>%
  mutate(caseflow = TRUE) %>%
  right_join(decisions, by = c("vacols_id" = "BFKEY")) %>%
  replace_na(list(caseflow = FALSE)) %>%
  mutate(month = as.Date(paste0(substr(BFDDEC, 1, 7), "-01"))) %>%
  group_by(month) %>%
  summarise(percent_caseflow = sum(caseflow) / n()) %>%
  filter(month < today() - months(1))

ggplot(by_month, aes(x = month, y = percent_caseflow, group = 1, label = percent(percent_caseflow))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  geom_line(color = "#0F2D52", size = 1) +
  geom_point(data = by_month[nrow(by_month),], color = "#0F2D52", size = 2) +
  geom_text(data = by_month[nrow(by_month),], vjust = -.6, size = 3, family = "Source Sans Pro", fontface = "bold", show.legend = FALSE) +
  dva_theme

ggsave("impact_reader_usage.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")

##########################
# Caseflow Hearings Prep #
##########################

hearings <- query("select FOLDER_NR, HEARING_PKSEQ, HEARING_DATE from HEARSCHED where HEARING_TYPE in ('C', 'T', 'V') and HEARING_DISP = 'H' and HEARING_DATE >= date '2017-10-01'") %>% mutate(HEARING_PKSEQ = as.character(HEARING_PKSEQ))
worksheets <- cfquery("select distinct appeals.vacols_id bfkey, hearings.vacols_id pkseq from hearings join appeals on appeals.id = appeal_id")

by_month <- worksheets %>%
  mutate(caseflow = TRUE) %>%
  right_join(hearings, by = c("bfkey" = "FOLDER_NR", "pkseq" = "HEARING_PKSEQ")) %>%
  replace_na(list(caseflow = FALSE)) %>%
  mutate(month = as.Date(paste0(substr(HEARING_DATE, 1, 7), "-01"))) %>%
  group_by(month) %>%
  summarise(percent_caseflow = sum(caseflow) / n()) %>%
  filter(month < today() - months(1))

ggplot(by_month, aes(x = month, y = percent_caseflow, group = 1, label = percent(percent_caseflow))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  geom_line(color = "#0F2D52", size = 1) +
  geom_point(data = by_month[nrow(by_month),], color = "#0F2D52", size = 2) +
  geom_text(data = by_month[nrow(by_month),], vjust = -.6, size = 3, family = "Source Sans Pro", fontface = "bold", show.legend = FALSE) +
  dva_theme

ggsave("impact_hp_usage.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")

###################
# Caseflow Intake #
###################

intakes <- cfquery("select receipt_date, detail_type, completed_at from intakes left join ramp_elections on detail_type = 'RampElection' and detail_id = ramp_elections.id where completion_status = 'success'") %>%
  mutate(control_time = as.numeric(as_date(completed_at) - as_date(receipt_date)))

nods.by_month <- query("select BFDNOD from BRIEFF where BFAC = '1' and BFDNOD >= date '2017-11-01'") %>%
  mutate(month = as_date(paste0(substr(BFDNOD, 1, 7), "-01"))) %>%
  count(month) %>%
  transmute(month, n_nods = n)

intakes.by_month <- intakes %>%
  mutate(month = as_date(paste0(substr(completed_at, 1, 7), "-01"))) %>%
  group_by(month) %>%
  summarise(
    n = n(),
    within_7 = sum(control_time <= 7, na.rm = TRUE) / sum(!is.na(control_time))
  ) %>%
  inner_join(nods.by_month, by = "month") %>%
  mutate(rate = n / (n_nods + n)) %>%
  filter(month < today() - months(1))

timeliness_target = .9

ggplot(intakes.by_month, aes(x = month, y = within_7, group = 1, label = percent(within_7))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  geom_line(color = "#0F2D52", size = 1) +
  geom_point(data = intakes.by_month[nrow(intakes.by_month),], color = "#0F2D52", size = 2) +
  geom_text(data = intakes.by_month[nrow(intakes.by_month),], vjust = -.6, size = 3, family = "Source Sans Pro", fontface = "bold", show.legend = FALSE) +
  geom_hline(yintercept = timeliness_target, color = "#9F2F3F", linetype = "dashed", alpha = 0.5) +
  geom_text(data = data.frame(within_7 = timeliness_target, month = max(intakes.by_month$month)), color = "#9F2F3F", hjust = 0, vjust = -.33, size = 3, family = "Source Sans Pro", fontface = "bold") +
  dva_theme

ggsave("impact_intake_timeliness.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")

ggplot(intakes.by_month, aes(x = month, y = n, label = comma(n))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 200)) +
  geom_bar(fill = "#0F2D52", stat = "identity") +
  geom_text(data = intakes.by_month[nrow(intakes.by_month),], vjust = -.6, size = 3, family = "Source Sans Pro", fontface = "bold") +
  dva_theme

ggsave("impact_intake_n.pdf", device = cairo_pdf, width = 5.5, height = 2, units = "in")
