# How often does mail arrive while an appeal is assigned to an attorney, and what are the consequences for timeliness?

assignments <- query("select DEFOLDER, DEASSIGN, MLRECVDATE, DERECEIVE, MLASSIGNEE, BFDDEC from DECASS join BRIEFF on DEFOLDER = BFKEY left join MAIL on DEFOLDER = MLFOLDER where DERECEIVE is not null and (MLRECVDATE is null or (MLRECVDATE >= DEASSIGN and MLRECVDATE < DERECEIVE)) and BFDDEC >= date '2015-10-01' and BFDDEC < date '2016-10-01'") %>%
  group_by(DEFOLDER) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  mutate(
    mail_interrupt = !is.na(MLRECVDATE),
    time_to_decision = as.numeric(as_date(BFDDEC) - as_date(DEASSIGN))
  )

assignment_sum <- assignments %>%
  group_by(mail_interrupt) %>%
  summarize(n = n(), time_to_decision = mean(time_to_decision))
