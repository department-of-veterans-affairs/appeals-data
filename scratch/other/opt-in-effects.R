as_date("2017-09-14") - (as_date("2019-02-14") - as_date("2017-10-02"))

pop <- query("
select
  BFD19,
  BFDDEC,
  BFDC,
  BFCURLOC
from BRIEFF
join FOLDER on BFKEY = TICKNUM
join CORRES on BFCORKEY = STAFKEY
left join (
  select FOLDER_NR, min(ADDTIME) ADDTIME
  from HEARSCHED
  where HEARING_TYPE in ('C', 'T', 'V')
  group by FOLDER_NR
) HEARSCHED on BFKEY = FOLDER_NR
left join (
  select ISSKEY, count(*) COMP_N
  from ISSUES
  where ISSPROG = '02'
  group by ISSKEY
) ISSUES on BFKEY = ISSKEY
where BFAC = '1'
  and BFHR in ('1', '2')
  and COMP_N > 0
  and (SDOB is null or SDOB > date '1941-05-02')
  and BF41STAT <= date '2016-05-02'
  and (TIDRECV is null or TIDRECV > date '2016-05-02')
  and (HEARSCHED.ADDTIME is null or HEARSCHED.ADDTIME > date '2016-05-02')
  and (BFDDEC is null or BFDDEC > date '2016-05-02')
  and (BFDC is not null or BFMPRO <> 'HIS')
")

oldest <- pop %>% arrange(BFD19) %>% filter(row_number() <= 2000)

sum(oldest$BFDC %in% c('1', '3', '4'), na.rm = TRUE) / nrow(oldest)


surv_pop <- query("
select
  BF41STAT,
  BFDDEC,
  BFDC
from BRIEFF
join CORRES on BFCORKEY = STAFKEY
where BFAC = '1'
  and (SDOB is null or SDOB > date '1941-05-02')
  and BF41STAT > date '2015-05-02'
  and BF41STAT <= date '2016-05-02'
  and (BFDC is not null or BFMPRO <> 'HIS')
") %>%
  mutate(
    age = as.numeric(as_date(ifelse(is.na(BFDDEC), as_date("2017-09-15"), as_date(BFDDEC))) - as_date(BF41STAT)),
    death = !is.na(BFDC) & BFDC %in% c('8', 'E')
  ) %>%
  filter(age > 0)

m <- survfit(Surv(age, death) ~ 1, data = surv_pop)
hazard <- 1 - summary(m, times = 500)$surv
dailyhazard <- hazard / 500

exposure <- oldest %>%
  filter(BFDC %in% c('1', '3', '4')) %>%
  mutate(exposure = as.numeric(as_date("2017-09-14") - as_date(BFDDEC))) %>%
  filter(exposure > 0)

sum(exposure$exposure) * dailyhazard / nrow(oldest)

