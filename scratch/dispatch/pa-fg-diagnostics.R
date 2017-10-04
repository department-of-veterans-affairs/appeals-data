# Dispatch diagnostics related to missing private attorney full grants

missing %<>%
  mutate(
    case_decision_date = as.Date(BFDDEC)
  )

write.table(unique(missing$BFCORLID), "sensitive_data/missing-pa-fg.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)

claims <- read.csv("sensitive_data/missing-pa-fg-claims.csv", stringsAsFactors = FALSE)

merged_decisions_claims <- missing %>%
  inner_join(claims, by = c("BFCORLID")) %>%
  mutate(
    claim_decision_date = as.Date(claim_receive_date, "%m/%d/%Y"),
    journal_date = as.Date(journal_date, "%m/%d/%Y"),
    precise_match = case_decision_date == claim_decision_date,
    fuzzy_match = !precise_match & abs(case_decision_date - claim_decision_date) <= 31
  ) %>%
  filter(precise_match | fuzzy_match)

decision_claim_matches <- merged_decisions_claims %>%
  select(BFCORLID, BFDDEC, precise_match, fuzzy_match) %>%
  group_by(BFCORLID, BFDDEC) %>%
  summarise(has_match = TRUE, has_precise_match = sum(precise_match) > 0)

merged_decisions_claims %<>%
  left_join(decision_claim_matches, by = c("BFCORLID", "BFDDEC")) %>%
  filter(precise_match | (fuzzy_match & !has_precise_match))

merged_decisions_claims$journal_date[merged_decisions_claims$journal_date < merged_decisions_claims$case_decision_date] <- NA

merged_decisions_claims %<>%
  group_by(BFCORLID, BFDDEC) %>%
  slice(which.min(ifelse(is.na(journal_date), 0, journal_date))) %>%
  select(BFCORLID, BFDDEC, has_match, has_precise_match, benefit_claim_id, claim_decision_date, journal_date, journal_object_id, journal_station, journal_user_id, claim_type_code, claim_type_name, end_product_type_code, status_type_code)

decisions <- missing %>%
  group_by(BFCORLID, BFDDEC) %>%
  mutate(multi_decision = n() > 1) %>%
  ungroup() %>%
  left_join(merged_decisions_claims, by = c("BFCORLID", "BFDDEC")) %>%
  mutate(
    time_to_ep = journal_date - case_decision_date,
    amc_ep = journal_station == 397
  )
