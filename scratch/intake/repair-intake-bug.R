source("R/vacolsConnect.R")

con <- vacolsConnect()
query <- function (query) { return(dbGetQuery(con, query)) }

library(dplyr)
library(glue)

vacolsConnect.archive <- function(dev = FALSE, password = NULL) {
  require("ROracle")

  username <- "DSUSER"
  password <- if(!is.null(password)) password
              else if(dev) Sys.getenv("vacols_dev_password") else Sys.getenv("vacols_db_password")
  sid      <- "ORCL"
  host     <- "localhost"
  port     <- 1522
  connect.string <- paste0(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
    "(CONNECT_DATA=(SID=", sid, ")))"
  )

  drv <- dbDriver("Oracle")
  con <- dbConnect(drv, username, password, dbname = connect.string)

  return(con)
}

con.archive <- vacolsConnect.archive()
query.archive <- function (query) { return(dbGetQuery(con.archive, query)) }

nonpostremands <- query("select BFKEY from BRIEFF join FOLDER on BFKEY = TICKNUM where BFDC = 'P' and BFDDEC < date '2018-01-05' and TIOCTIME is null")
postremands <- query("select BFKEY from BRIEFF join FOLDER on BFKEY = TICKNUM where BFDC = 'P' and BFDDEC < date '2018-01-05' and TIOCTIME is not null")

nonpostremandbfkeystr <- paste(nonpostremands$BFKEY, collapse = "','")
overwrittendisps <- query.archive(glue("select ISSKEY, ISSSEQ, ISSDC, ISSDCLS from ISSUES where ISSKEY in ('{nonpostremandbfkeystr}') and ISSDC <> 'P'"))

for (i in 1:nrow(overwrittendisps)) {
  row <- overwrittendisps[i,]
  print(glue("repairing {row$ISSKEY}-{row$ISSSEQ}"))

  valid = TRUE

  valid <- valid & query(glue("update ISSUES set ISSDC = '{row$ISSDC}', ISSDCLS = date '{format(row$ISSDCLS, format='%Y-%m-%d')}' where ISSKEY = '{row$ISSKEY}' and ISSSEQ = '{row$ISSSEQ}'"))

  # if (valid) {
  #   valid <- valid & query("commit")
  # }

  if (!valid) {
    print(glue("problem repairing {row$ISSKEY}-{row$ISSSEQ}"))
    break
  }
}

da <- read.csv("da_issues.csv")
relevant_da <- filter(da, TICKNUM %in% c('2048550', '2645353')) %>% select(TICKNUM, ISSSEQ, ISSDC, ISSDCLS, BFMEMID, DEATTY)

postremandbfkeystr <- paste(postremands$BFKEY, collapse = "','")
brieff_recovered <- query.archive(glue("select BFKEY, BFDDEC, BFDC, BFBOARD, BFMEMID, BFATTID from BRIEFF where BFKEY in ('{postremandbfkeystr}') and BFDC <> 'P'")) %>% mutate(BFDDEC = as_date(BFDDEC))
issues_recovered <- query.archive(glue("select ISSKEY, ISSSEQ, ISSDC, ISSDCLS from ISSUES where ISSKEY in ('{postremandbfkeystr}') and ISSDC <> 'P'")) %>% mutate(ISSDCLS = as_date(ISSDCLS))

additional_brieff <- data.frame(BFKEY = c('2048550', '2645353'), BFDDEC = c(as_date('2012-03-07'), as_date('2012-12-18')), BFDC = c('1', '1'), BFBOARD = c('D3', 'A4'), BFMEMID = c('731', '731'), BFATTID = c('582', '985'), stringsAsFactors = FALSE)
additional_issues <- filter(da, TICKNUM %in% c('2048550', '2645353')) %>% transmute(ISSKEY = TICKNUM, ISSSEQ, ISSDC, ISSDCLS = as_date(ISSDCLS))

folders_to_fix <- rbind(brieff_recovered, additional_brieff)
issues_to_fix <- rbind(issues_recovered, additional_issues)

rampbfkeystr <- paste(folders_to_fix$BFKEY, collapse = "','")
folders_to_ramp <- query(glue("select BFKEY, BFDLOOUT, TIMDUSER, BFCORKEY, BFCORLID, BFDNOD, BFDSOC, BFD19, BF41STAT, BFREGOFF, BFISSNR, BFDORG, BFDC, BFIC, BFIO, BFOC, BFMS, BFSH, BFSO, BFST, BFDRODEC, BFCASEV, BFDPDCN, BFDDRO, BFDROID, BFDRORTR, BFRO1, TICORKEY, TISTKEY, TITRNUM, TINUM, TIADTIME, TIAGOR, TIASBT, TIGWUI, TIHEPC, TIAIDS, TIMGAS, TIPTSD, TIRADB, TIRADN, TISARC, TISEXH, TITOBA, TINOSC, TI38US, TINNME, TINWGR, TIPRES, TITRTM, TINOOT from BRIEFF join FOLDER on BFKEY = TICKNUM where BFKEY in ('{rampbfkeystr}')"))
write.csv(folders_to_ramp, "folders_to_ramp.csv", row.names = FALSE)

for (i in 1:nrow(folders_to_fix)) {
  row <- folders_to_fix[i,]
  print(glue("repairing {row$BFKEY}"))

  valid = TRUE

  valid <- valid & query(glue("update BRIEFF set BFDC = '{row$BFDC}', BFDDEC = date '{format(row$BFDDEC, format='%Y-%m-%d')}', BFBOARD = '{row$BFBOARD}', BFMEMID = '{row$BFMEMID}', BFATTID = '{row$BFATTID}' where BFKEY = '{row$BFKEY}'"))
  valid <- valid & query(glue("update FOLDER set TIDCLS = date '{format(row$BFDDEC, format='%Y-%m-%d')}' where TICKNUM = '{row$BFKEY}'"))

  # if (valid) {
  #   valid <- valid & query("commit")
  # }

  if (!valid) {
    print(glue("problem repairing {row$BFKEY}"))
    break
  }
}

for (i in 1:nrow(issues_to_fix)) {
  row <- issues_to_fix[i,]
  print(glue("repairing {row$ISSKEY}-{row$ISSSEQ}"))

  valid = TRUE

  valid <- valid & query(glue("update ISSUES set ISSDC = '{row$ISSDC}', ISSDCLS = date '{format(row$ISSDCLS, format='%Y-%m-%d')}' where ISSKEY = '{row$ISSKEY}' and ISSSEQ = '{row$ISSSEQ}'"))

  # if (valid) {
  #   valid <- valid & query("commit")
  # }

  if (!valid) {
    print(glue("problem repairing {row$ISSKEY}-{row$ISSSEQ}"))
    break
  }
}

folders_to_ramp <- read.csv("folders_to_ramp.csv", colClasses = "character", stringsAsFactors = FALSE) %>%
  mutate(
    BFDLOOUT = as.POSIXct(BFDLOOUT),
    BFDNOD = as.POSIXct(BFDNOD),
    BFDSOC = as.POSIXct(BFDSOC),
    BFD19 = as.POSIXct(BFD19),
    BF41STAT = as.POSIXct(BF41STAT),
    BFDORG = as.POSIXct(BFDORG),
    BFDRODEC = as.POSIXct(BFDRODEC),
    BFDPDCN = as.POSIXct(BFDPDCN),
    BFDDRO = as.POSIXct(BFDDRO),
    TIADTIME = as.POSIXct(TIADTIME)
  )
rampbfkeystr <- paste(folders_to_ramp$BFKEY, collapse = "','")
issues_to_ramp <- query(glue("select ISSKEY, ISSPROG, ISSDC, ISSCODE, ISSLEV1, ISSLEV2, ISSLEV3, ISSDESC, ISSGR from ISSUES where ISSKEY in ('{rampbfkeystr}') order by ISSKEY, ISSSEQ"))
write.csv(issues_to_ramp, "issues_to_ramp.csv", row.names = FALSE)

for (i in 1:nrow(folders_to_ramp)) {
  row <- folders_to_ramp[i,]
  issues <- filter(issues_to_ramp, ISSKEY == row$BFKEY, ISSDC == "3")
  print(glue("repairing {row$BFKEY}"))

  valid = TRUE

  brieff <- row %>% select(BFCORKEY, BFCORLID, BFDNOD, BFDSOC, BFD19, BF41STAT, BFREGOFF, BFISSNR, BFDORG, BFIC, BFIO, BFOC, BFMS, BFSH, BFSO, BFST, BFDRODEC, BFCASEV, BFDPDCN, BFDDRO, BFDROID, BFDRORTR, BFRO1)
  brieff_colnames <- c(c("BFKEY", "BFMPRO", "BFDDEC", "BFDC", "BFBOARD", "BFATTID", "BFAC", "BFCURLOC", "BFDLOOUT", "BFDLOCIN"), colnames(brieff[,which(!is.na(brieff))]))

  brieff_colname_str <- paste(brieff_colnames, collapse = ", ")

  brieff_values <- paste0("'", row$BFKEY, "P'")
  brieff_values <- brieff_values %>% c("'HIS'")
  brieff_values <- brieff_values %>% c(glue("TO_DATE('{format(row$BFDLOOUT, format='%Y-%m-%d %H:%M:%S')}', 'YYYY-MM-DD HH24:MI:SS')"))
  brieff_values <- brieff_values %>% c("'P'")
  brieff_values <- brieff_values %>% c("'88'")
  brieff_values <- brieff_values %>% c("'888'")
  brieff_values <- brieff_values %>% c("'3'")
  brieff_values <- brieff_values %>% c("'99'")
  brieff_values <- brieff_values %>% c(glue("TO_DATE('{format(row$BFDLOOUT, format='%Y-%m-%d %H:%M:%S')}', 'YYYY-MM-DD HH24:MI:SS')"))
  brieff_values <- brieff_values %>% c(glue("TO_DATE('{format(row$BFDLOOUT, format='%Y-%m-%d %H:%M:%S')}', 'YYYY-MM-DD HH24:MI:SS')"))

  if (!is.na(brieff$BFCORKEY)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFCORKEY}'")) }
  if (!is.na(brieff$BFCORLID)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFCORLID}'")) }
  if (!is.na(brieff$BFDNOD)) { brieff_values <- brieff_values %>% c(glue("date '{format(brieff$BFDNOD, format='%Y-%m-%d')}'")) }
  if (!is.na(brieff$BFDSOC)) { brieff_values <- brieff_values %>% c(glue("date '{format(brieff$BFDSOC, format='%Y-%m-%d')}'")) }
  if (!is.na(brieff$BFD19)) { brieff_values <- brieff_values %>% c(glue("date '{format(brieff$BFD19, format='%Y-%m-%d')}'")) }
  if (!is.na(brieff$BF41STAT)) { brieff_values <- brieff_values %>% c(glue("date '{format(brieff$BF41STAT, format='%Y-%m-%d')}'")) }
  if (!is.na(brieff$BFREGOFF)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFREGOFF}'")) }
  if (!is.na(brieff$BFISSNR)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFISSNR}'")) }
  if (!is.na(brieff$BFDORG)) { brieff_values <- brieff_values %>% c(glue("TO_DATE('{format(brieff$BFDORG, format='%Y-%m-%d %H:%M:%S')}', 'YYYY-MM-DD HH24:MI:SS')")) }
  if (!is.na(brieff$BFIC)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFIC}'")) }
  if (!is.na(brieff$BFIO)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFIO}'")) }
  if (!is.na(brieff$BFOC)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFOC}'")) }
  if (!is.na(brieff$BFMS)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFMS}'")) }
  if (!is.na(brieff$BFSH)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFSH}'")) }
  if (!is.na(brieff$BFSO)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFSO}'")) }
  if (!is.na(brieff$BFST)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFST}'")) }
  if (!is.na(brieff$BFDRODEC)) { brieff_values <- brieff_values %>% c(glue("date '{format(brieff$BFDRODEC, format='%Y-%m-%d')}'")) }
  if (!is.na(brieff$BFCASEV)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFCASEV}'")) }
  if (!is.na(brieff$BFDPDCN)) { brieff_values <- brieff_values %>% c(glue("date '{format(brieff$BFDPDCN, format='%Y-%m-%d')}'")) }
  if (!is.na(brieff$BFDDRO)) { brieff_values <- brieff_values %>% c(glue("date '{format(brieff$BFDDRO, format='%Y-%m-%d')}'")) }
  if (!is.na(brieff$BFDROID)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFDROID}'")) }
  if (!is.na(brieff$BFDRORTR)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFDRORTR}'")) }
  if (!is.na(brieff$BFRO1)) { brieff_values <- brieff_values %>% c(glue("'{brieff$BFRO1}'")) }

  brieff_value_str <- paste(brieff_values, collapse = ", ")

  folder <- row %>% select(TICORKEY, TISTKEY, TITRNUM, TINUM, TIADTIME, TIAGOR, TIASBT, TIGWUI, TIHEPC, TIAIDS, TIMGAS, TIPTSD, TIRADB, TIRADN, TISARC, TISEXH, TITOBA, TINOSC, TI38US, TINNME, TINWGR, TIPRES, TITRTM, TINOOT)
  folder_colnames <- c(c("TICKNUM", "TICUKEY", "TIKEYWRD", "TIMDTIME", "TIMDUSER"), colnames(folder[,which(!is.na(folder))]))

  folder_colname_str <- paste(folder_colnames, collapse = ", ")

  folder_values <- paste0("'", row$BFKEY, "P'")
  folder_values <- folder_values %>% c("'HISTORY'")
  folder_values <- folder_values %>% c("'HISTORY'")
  folder_values <- folder_values %>% c(glue("TO_DATE('{format(row$BFDLOOUT, format='%Y-%m-%d %H:%M:%S')}', 'YYYY-MM-DD HH24:MI:SS')"))
  folder_values <- folder_values %>% c(glue("'{row$TIMDUSER}'"))

  if (!is.na(folder$TICORKEY)) { folder_values <- folder_values %>% c(glue("'{folder$TICORKEY}'")) }
  if (!is.na(folder$TISTKEY)) { folder_values <- folder_values %>% c(glue("'{folder$TISTKEY}'")) }
  if (!is.na(folder$TITRNUM)) { folder_values <- folder_values %>% c(glue("'{folder$TITRNUM}'")) }
  if (!is.na(folder$TINUM)) { folder_values <- folder_values %>% c(glue("'{folder$TINUM}'")) }
  if (!is.na(folder$TIADTIME)) { folder_values <- folder_values %>% c(glue("date '{format(folder$TIADTIME, format='%Y-%m-%d')}'")) }
  if (!is.na(folder$TIAGOR)) { folder_values <- folder_values %>% c(glue("'{folder$TIAGOR}'")) }
  if (!is.na(folder$TIASBT)) { folder_values <- folder_values %>% c(glue("'{folder$TIASBT}'")) }
  if (!is.na(folder$TIGWUI)) { folder_values <- folder_values %>% c(glue("'{folder$TIGWUI}'")) }
  if (!is.na(folder$TIHEPC)) { folder_values <- folder_values %>% c(glue("'{folder$TIHEPC}'")) }
  if (!is.na(folder$TIAIDS)) { folder_values <- folder_values %>% c(glue("'{folder$TIAIDS}'")) }
  if (!is.na(folder$TIMGAS)) { folder_values <- folder_values %>% c(glue("'{folder$TIMGAS}'")) }
  if (!is.na(folder$TIPTSD)) { folder_values <- folder_values %>% c(glue("'{folder$TIPTSD}'")) }
  if (!is.na(folder$TIRADB)) { folder_values <- folder_values %>% c(glue("'{folder$TIRADB}'")) }
  if (!is.na(folder$TIRADN)) { folder_values <- folder_values %>% c(glue("'{folder$TIRADN}'")) }
  if (!is.na(folder$TISARC)) { folder_values <- folder_values %>% c(glue("'{folder$TISARC}'")) }
  if (!is.na(folder$TISEXH)) { folder_values <- folder_values %>% c(glue("'{folder$TISEXH}'")) }
  if (!is.na(folder$TITOBA)) { folder_values <- folder_values %>% c(glue("'{folder$TITOBA}'")) }
  if (!is.na(folder$TINOSC)) { folder_values <- folder_values %>% c(glue("'{folder$TINOSC}'")) }
  if (!is.na(folder$TI38US)) { folder_values <- folder_values %>% c(glue("'{folder$TI38US}'")) }
  if (!is.na(folder$TINNME)) { folder_values <- folder_values %>% c(glue("'{folder$TINNME}'")) }
  if (!is.na(folder$TINWGR)) { folder_values <- folder_values %>% c(glue("'{folder$TINWGR}'")) }
  if (!is.na(folder$TIPRES)) { folder_values <- folder_values %>% c(glue("'{folder$TIPRES}'")) }
  if (!is.na(folder$TITRTM)) { folder_values <- folder_values %>% c(glue("'{folder$TITRTM}'")) }
  if (!is.na(folder$TINOOT)) { folder_values <- folder_values %>% c(glue("'{folder$TINOOT}'")) }

  folder_value_str <- paste(folder_values, collapse = ", ")

  priorloc_colname_str <- "LOCKEY, LOCDOUT, LOCDTO, LOCSTTO, LOCSTOUT"
  priorloc_value_str <- glue("'{paste0(row$BFKEY, 'P')}', TO_DATE('{format(row$BFDLOOUT, format='%Y-%m-%d %H:%M:%S')}', 'YYYY-MM-DD HH24:MI:SS'), TO_DATE('{format(row$BFDLOOUT, format='%Y-%m-%d %H:%M:%S')}', 'YYYY-MM-DD HH24:MI:SS'), '99', '{row$TIMDUSER}'")

  valid <- valid & query(glue("insert into BRIEFF ({brieff_colname_str}) values ({brieff_value_str})"))
  valid <- valid & query(glue("insert into FOLDER ({folder_colname_str}) values ({folder_value_str})"))
  valid <- valid & query(glue("insert into PRIORLOC ({priorloc_colname_str}) values ({priorloc_value_str})"))

  for (j in 1:nrow(issues)) {
    issue <- issues[j,] %>% select(ISSPROG, ISSCODE, ISSLEV1, ISSLEV2, ISSLEV3, ISSDESC, ISSGR)

    issues_colnames <- c(c("ISSKEY", "ISSSEQ", "ISSDC", "ISSDCLS", "ISSADTIME", "ISSADUSER"), colnames(issue[,which(!is.na(issue))]))
    issues_colname_str <- paste(issues_colnames, collapse = ", ")

    issue_values <- paste0("'", row$BFKEY, "P'")
    issue_values <- issue_values %>% c(j)
    issue_values <- issue_values %>% c("'P'")
    issue_values <- issue_values %>% c(glue("TO_DATE('{format(row$BFDLOOUT, format='%Y-%m-%d %H:%M:%S')}', 'YYYY-MM-DD HH24:MI:SS')"))
    issue_values <- issue_values %>% c(glue("TO_DATE('{format(row$BFDLOOUT, format='%Y-%m-%d %H:%M:%S')}', 'YYYY-MM-DD HH24:MI:SS')"))
    issue_values <- issue_values %>% c(glue("'{row$TIMDUSER}'"))

    if (!is.na(issue$ISSPROG)) { issue_values <- issue_values %>% c(glue("'{issue$ISSPROG}'")) }
    if (!is.na(issue$ISSCODE)) { issue_values <- issue_values %>% c(glue("'{issue$ISSCODE}'")) }
    if (!is.na(issue$ISSLEV1)) { issue_values <- issue_values %>% c(glue("'{issue$ISSLEV1}'")) }
    if (!is.na(issue$ISSLEV2)) { issue_values <- issue_values %>% c(glue("'{issue$ISSLEV2}'")) }
    if (!is.na(issue$ISSLEV3)) { issue_values <- issue_values %>% c(glue("'{issue$ISSLEV3}'")) }
    if (!is.na(issue$ISSDESC)) { issue_values <- issue_values %>% c(glue("'{issue$ISSDESC}'")) }
    if (!is.na(issue$ISSGR)) { issue_values <- issue_values %>% c(glue("'{issue$ISSGR}'")) }

    issues_value_str <- paste(issue_values, collapse = ", ")

    valid <- valid & query(glue("insert into ISSUES ({issues_colname_str}) values ({issues_value_str})"))
  }

  # if (valid) {
  #   valid <- valid & query("commit")
  # }

  if (!valid) {
    print(glue("problem repairing {row$BFKEY}"))
    break
  }
}
