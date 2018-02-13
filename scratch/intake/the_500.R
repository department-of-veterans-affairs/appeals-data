list <- read.csv("sensitive_data/the_500.csv", stringsAsFactors = FALSE, colClasses = c("character"), strip.white = TRUE)

list$corlid <- ifelse(nchar(list$id) == 9, paste0(list$id, "S"), paste0(as.integer(list$id), "C"))

corlids <- query(paste0("select BFKEY, BFCORLID from BRIEFF where BFKEY in ('", paste(list$folder, collapse = "','"),"')"))

list <- left_join(list, corlids, by = c("folder" = "BFKEY"))

list$folder %>% unique() %>% length()

vets <- query(paste0("select BFCORLID, count(*) from BRIEFF where BFMPRO <> 'HIS' and BFCORLID in ('", paste(list$corlid, collapse = "','"),"') group by BFCORLID"))

age <- query(paste0("select BFKEY, BFCORLID, SDOB, SSPARE2 from BRIEFF join CORRES on BFCORKEY = STAFKEY where BFKEY in ('", paste(list$folder, collapse = "','"),"')")) %>%
  filter(is.na(SSPARE2))

sum(as_date(age$SDOB) <= as_date("1942-10-26"), na.rm = TRUE)
sum(as_date(age$SDOB) <= as_date("1932-10-26"), na.rm = TRUE)

rep <- query(paste0("select BFKEY, BFCORLID, BFSO from BRIEFF where BFKEY in ('", paste(list$folder, collapse = "','"),"')"))

list$id %>% unique() %>% length()

list[list$corlid != list$BFCORLID,]

issues <- rep <- query(paste0("select BFKEY, ISSPROG, count(*) from BRIEFF join ISSUES on BFKEY = ISSKEY where BFKEY in ('", paste(list$folder, collapse = "','"),"') group by BFKEY, ISSPROG"))
mixed_appeals <- rep <- query(paste0("select BFKEY, count(case when ISSPROG = '02' then 1 end), sum(case when ISSPROG <> '02' then 1 end) from BRIEFF join ISSUES on BFKEY = ISSKEY and BFKEY in ('", paste(list$folder, collapse = "','"),"') group by BFKEY"))

aod <- query(paste0("select BFKEY, BFMPRO, VACOLS.AOD_CNT(BFKEY) from BRIEFF where BFKEY in ('", paste(list$folder, collapse = "','"),"')"))

act <- query(paste0("select BFKEY, BFMPRO, BFDC from BRIEFF where BFKEY in ('", paste(list$folder, collapse = "','"),"')"))

bfcurloc <- query(paste0("select BFKEY, BFDDEC, BFMPRO, BFCURLOC from BRIEFF where BFKEY in ('", paste(list$folder, collapse = "','"),"')")) %>% filter(!(BFCURLOC %in% c("50", "77", "99")))

appellant <- query(paste0("select BFKEY, SSPARE1, SSPARE2 from BRIEFF join CORRES on BFCORKEY = STAFKEY where SSPARE2 is not null and BFKEY in ('", paste(list$folder, collapse = "','"),"')"))

hearings <- query(paste0("select BFKEY, HEARING_TYPE, HEARING_DATE from BRIEFF join HEARSCHED on BFKEY = FOLDER_NR where HEARING_TYPE in ('C', 'T', 'V') and HEARING_DISP is null and BFKEY in ('", paste(list$folder, collapse = "','"),"')"))

