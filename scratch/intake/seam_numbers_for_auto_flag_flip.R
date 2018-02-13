hearing_seam <- query("select BFKEY, BFCORLID from BRIEFF join FOLDER on BFKEY = TICKNUM where BFMPRO = 'ADV' and BF41STAT is not null and BFHR in ('1', '2') and TIVBMS = 'Y'")

nonhearing_seam <- query("select BFKEY, BFCORLID, BFD19, BF41STAT, BFDCERTOOL from BRIEFF join FOLDER on BFKEY = TICKNUM where BFMPRO = 'ADV' and BF41STAT is not null and BFHR = '5' and TIVBMS = 'Y'")

certifications <- cfQuery("select vacols_id, v2 from certifications") %>%
  group_by(vacols_id) %>%
  summarize(v2 = any(v2))

test <- nonhearing_seam %>%
  left_join(certifications, by = c("BFKEY" = "vacols_id"))

sum(!is.na(test$BFDCERTOOL)) / nrow(test)
sum(test$v2, na.rm = TRUE) / nrow(test)
sum(test$v2, na.rm = TRUE)

v2 <- subset(test, v2)
quantile(v2$BFD19, probs = c(0.05, .95))
