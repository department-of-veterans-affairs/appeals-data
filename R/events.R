####################################
####### VACOLS QUERY HELPERS #######
####################################

#' Retrieve a log of events that appear in VACOLS as date columns.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param cols Vector of column names to include in results.
#' @param labs (optional) Vector of labels to be used for the \code{EVENT_TYPE}
#'   variable.
#' @param join (optional) SQL statement to join a table other than BRIEFF.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#'
#' @examples
#' cols <- c("BFNOD", "BFDSOC", "BFD19")
#' labs <- c("NOD", "SOC", "FORM9")
#' event_getDateCols(con, cols, labs, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
#'
#' event_getDateCols(con, c("HEARING_DATE"), c("hearing"), join = "HEARSCHED on FOLDER_NR = BFKEY", where = "HEARING_DATE > date '2016-10-01'")
event_getDateCols <- function(con, cols, labs, join, where) {
  if(missing(labs)) labs <- cols
  source("R/constants.R")

  require("magrittr")
  require("dplyr")
  require("tidyr")

  aliases <- sapply(seq_along(cols), function(i) { return(paste(cols[i], "as", labs[i])) })
  nullTests <- sapply(cols, function(x) { return(paste(x, "is not null")) })

  query <- c(
    "select BFCORLID, BFDNOD as APPEAL_DATE, BFKEY,", paste(aliases, collapse = ", "),
    "from BRIEFF"
  )
  if(!missing(join)) query %<>% c("join", join)
  query %<>% c(
    "where (", paste(nullTests, collapse = " or "), ")",
    "and", EventCaseExclusions
  )
  if(!missing(where)) query %<>% c("and", where)
  query %<>% paste(collapse = " ")

  print(paste("Querying VACOLS:", query))

  result <- dbGetQuery(con, query) %>%
    gather(EVENT_TYPE, DATE, -BFCORLID, -APPEAL_DATE, -BFKEY) %>%
    filter(!is.na(DATE))

  return (result)
}


#' Retrieve a log of events from the VACOLS PRIORLOC table.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{LOCDOUT}, \code{LOCDIN},
#'   \code{LOCSTTO}, \code{LOCSTOUT}, \code{LOCSTRCV}, \code{LOC} and
#'   \code{LOC_NAME}.#'
#' @examples
#' event_getPriorLocs(con, "LOCSTTO = '48' and LOCSTOUT > date '2015-01-01' and LOCSTOUT < date '2015-12-31'")
event_getPriorLocs <- function(con, join, where) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  query <- c(
    "select BFCORLID, BFDNOD as APPEAL_DATE, BFKEY, LOCDOUT, LOCDIN, LOCSTTO, LOCSTOUT, LOCSTRCV",
    "from BRIEFF",
    "join PRIORLOC on LOCKEY = BFKEY"
  )
  if(!missing(join)) query %<>% c("join", join)

  query %<>% c("where", EventCaseExclusions)
  if(!missing(where)) query %<>% c("and", where)

  query %<>% paste(collapse = " ")

  print(paste("Querying VACOLS:", query))
  result <- dbGetQuery(con, query)

  print(paste("Querying VACOLS:", "select STAFKEY, SNAMEL, STITLE from STAFF"))
  staff <- dbGetQuery(con, "select STAFKEY, SNAMEL, STITLE from STAFF")

  staff %<>%
    mutate(LOC = ifelse(is.na(STITLE), STAFKEY, STITLE)) %>%
    left_join(staff, by = c("LOC" = "STAFKEY")) %>%
    select(STAFKEY, LOC, LOC_NAME = matches("SNAMEL.y"))

  result %<>%
    left_join(staff, by = c("LOCSTTO" = "STAFKEY")) %>%
    arrange(BFCORLID, APPEAL_DATE, LOCDOUT)

  return (result)
}


######################################
####### PRIOR LOCATION PARSING #######
######################################

.parseTranslationLocs <- function(loclog) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")
  require("tidyr")

  request <- loclog %>%
    filter(LOC %in% c(TransRequestLoc)) %>%
    mutate(EVENT_TYPE = "TRANSLATION_REQ") %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = LOCDOUT)

  abeyance <- loclog %>%
    filter(LOC %in% c(TransAbeyanceLoc)) %>%
    mutate(
      TO_TRANSLATION = LOCDOUT,
      FROM_TRANSLATION = LOCDIN
    ) %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, TO_TRANSLATION, FROM_TRANSLATION) %>%
    gather(EVENT_TYPE, DATE, -BFCORLID, -APPEAL_DATE, -BFKEY)

  return(rbind(request, abeyance))
}


.parseVSOLocs <- function(loclog) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  loclog %<>%
    arrange(BFCORLID, APPEAL_DATE, LOCDOUT) %>%
    mutate(LOC_PLUS1 = ifelse(
      loclog$BFCORLID == c(tail(loclog$BFCORLID, -1), rep(NA, 1)) &
        loclog$APPEAL_DATE == c(tail(loclog$APPEAL_DATE, -1), rep(NA, 1)),
      c(tail(loclog$LOC, -1), rep(NA, 1)), NA
    ))

  toVSO <- loclog %>%
    filter(
      LOC != VSOLoc,
      LOC_PLUS1 == VSOLoc
    ) %>%
    mutate(EVENT_TYPE = "TO_VSO") %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = LOCDIN)

  fromVSO <- loclog %>%
    filter(
      LOC == VSOLoc,
      LOC_PLUS1 != VSOLoc
    ) %>%
    mutate(EVENT_TYPE = "FROM_VSO") %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = LOCDOUT)

  return(rbind(toVSO, fromVSO))
}


.parseAbeyanceLocs <- function(loclog) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")
  require("tidyr")

  loclog %>%
    filter(grepl(AbeyanceLocs, LOC)) %>%
    mutate(TO_ABEYANCE = LOCDOUT, FROM_ABEYANCE = LOCDIN) %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, TO_ABEYANCE, FROM_ABEYANCE) %>%
    gather(EVENT_TYPE, DATE, -BFCORLID, -APPEAL_DATE, -BFKEY) %>%
    return
}


.parseOMOLocs <- function(loclog) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")
  require("tidyr")

  loclog %>%
    arrange(BFCORLID, APPEAL_DATE, LOCDOUT) %>%
    mutate(LOC_MINUS1 = ifelse(
      loclog$BFCORLID == c(rep(NA, 1), head(loclog$BFCORLID, -1)) &
        loclog$APPEAL_DATE == c(rep(NA, 1), head(loclog$APPEAL_DATE, -1)),
      c(rep(NA, 1), head(loclog$LOC, -1)), NA
    )) %>%
    filter(
      LOC == OutsideBVALoc,
      LOC_MINUS1 == OMORequestLoc
    ) %>%
    mutate(TO_OMO = LOCDOUT, FROM_OMO = LOCDIN) %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, TO_OMO, FROM_OMO) %>%
    gather(EVENT_TYPE, DATE, -BFCORLID, -APPEAL_DATE, -BFKEY) %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE) %>%
    return
}


.parseDecisionLocs <- function(loclog) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  result <- loclog %>%
    group_by(BFKEY, cumsum(!grepl(DispatchLocs, LOC, perl = TRUE))) %>%
    arrange(desc(LOCDOUT)) %>%
    mutate(dispatch_rows = row_number()) %>%
    ungroup() %>%
    arrange(BFKEY, LOCDOUT)

  result$storage_loc <- result$LOC[1:nrow(result) + result$dispatch_rows]

  result %<>%
    filter(
      dispatch_rows > 0,
      storage_loc == CentralDispatchLoc,
      grepl(DecisionLocs, LOC, perl = TRUE)
    ) %>%
    mutate(EVENT_TYPE = "SIGNED_DECISION") %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = LOCDIN) %>%
    group_by(BFKEY) %>%
    arrange(desc(DATE)) %>%
    filter(row_number() == 1) %>%
    ungroup()

  return(result)
}


.parseQRLocs <- function(loclog) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  loclog %>%
    filter(LOC == QRLoc) %>%
    mutate(EVENT_TYPE = "QR") %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = LOCDIN) %>%
    return
}


.parseRemReturnLocs <- function(loclog) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  loclog %>%
    filter(LOC == RemReturnLoc) %>%
    mutate(EVENT_TYPE = "REMAND_RETURN") %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = LOCDOUT) %>%
    group_by(BFKEY) %>%
    arrange(DATE) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    return
}


##################################
####### GET EVENTS OF TYPE #######
##################################

#' Retrieve a log of Notice of Disagreement (NOD) dates matching the specified
#' criteria.
#'
#' @section Methodology: Uses the date from the BFDNOD column, limited to
#'   appeals where the Type Action (BFAC) indicates that it is the original
#'   action (1). Merged appeals and dummy data are also excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_nod(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_nod <- function (con, where) {
  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("BFDNOD"), c("NOD"), where = where))
}


#' Retrieve a log of VACOLS Creation (VACOLS) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the minimum LOCDOUT timestamp from the PRIORLOC
#'   table. Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_vacolsCreation(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_vacolsCreation <- function (con, join, where) {
  join <- paste0("(select LOCKEY, min(LOCDOUT) VACOLS from PRIORLOC group by LOCKEY) on LOCKEY = BFKEY",
                 ifelse(missing(join), "", paste0(" join ", join)))

  return(event_getDateCols(con, c("VACOLS"), join = join, where = where))
}


#' Retrieve a log of Substitution (SUBSTITUTION) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the minimum LOCDOUT timestamp from the PRIORLOC
#'   table for appeals where BFSUB is "S". Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_substitution(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_substitution <- function (con, join, where) {
  join <- paste0("(select LOCKEY, min(LOCDOUT) SUBSTITUTION from PRIORLOC group by LOCKEY) on LOCKEY = BFKEY",
                 ifelse(missing(join), "", paste0(" join ", join)))

  where <- paste0(
    "BFSUB = 'S'",
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("SUBSTITUTION"), join = join, where = where))
}


#' Retrieve a log of Statement of Case (SOC) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the date from the BFDSOC column, limited to
#'   appeals where the Type Action (BFAC) indicates that it is the original
#'   action (1). Merged appeals and dummy data are also excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_soc(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_soc <- function (con, where) {
  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("BFDSOC"), c("SOC"), where = where))
}


#' Retrieve a log of Form 9 (FORM9) events matching the specified criteria.
#'
#' @section Methodology: Uses the date from the BFD19 column, limited to appeals
#'   where the Type Action (BFAC) indicates that it is the original action (1).
#'   Merged appeals and dummy data are also excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_form9(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_form9 <- function (con, where) {
  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("BFD19"), c("FORM9"), where = where))
}


#' Retrieve a log of Supplemental Statement of Case (SSOC) events matching the
#' specified criteria.
#'
#' @section Methodology: Uses the dates from the BFSSOC1, BFSSOC2, BFSSOC3,
#'   BFSSOC4, and BFSSOC5 columns from any appeal, uniqued by the BFCORLID.
#'   Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_ssoc(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_ssoc <- function (con, join, where) {
  require("magrittr")
  require("dplyr")

  cols <- c("BFSSOC1", "BFSSOC2", "BFSSOC3", "BFSSOC4", "BFSSOC5")

  result <- event_getDateCols(con, cols, join = join, where = where) %>%
    mutate(EVENT_TYPE = 'SSOC') %>%
    distinct(BFCORLID, DATE, .keep_all = TRUE)

  return(result)
}


#' Retrieve a log of Certification to BVA (CERTIFICATION) events matching the
#' specified criteria.
#'
#' @section Methodology: Uses the date from the BF41STAT column, limited to
#'   appeals where the Type Action (BFAC) indicates that it is the original
#'   action (1). Merged appeals and dummy data are also excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_certification(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_certification <- function (con, where) {
  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("BF41STAT"), c("CERTIFICATION"), where = where))
}


#' Retrieve a log of Assigned to Docket (DOCKET) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the date from the TIADTIME column on the FOLDER
#'   table, limited to appeals where the Type Action (BFAC) indicates that it is
#'   the original action (1). Merged appeals and dummy data are also excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_docket(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_docket <- function (con, where) {
  join <- "FOLDER on BFKEY = TICKNUM"

  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("TIADTIME"), c("DOCKET"), join = join, where = where))
}


#' Retrieve a log of Activated at BVA (ACTIVATION) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the date from the TIDRECV column on the FOLDER
#'   table. Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_activation(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_activation <- function (con, where) {
  join <- "FOLDER on BFKEY = TICKNUM"

  return(event_getDateCols(con, c("TIDRECV"), c("ACTIVATION"), join = join, where = where))
}


#' Retrieve a log of Case Review (CASE_REVIEW) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the date from the TIDKTIME column on the FOLDER
#'   table. Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_review(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_review <- function (con, where) {
  join <- "FOLDER on BFKEY = TICKNUM"

  return(event_getDateCols(con, c("TIDKTIME"), c("CASE_REVIEW"), join = join, where = where))
}


#' Retrieve a log of Hearing Held (HEARING) and Hearing Not Held (HEARING_EXCEPTION)
#' events matching the specified criteria.
#'
#' @section Methodology: Uses the date from the HEARING_DATE column on the
#'   FOLDER table, excluding formal hearings (HEARING_DISP = "F"), and RO formal
#'   hearings (HEARING_TYPE <> "F"). Hearings where the HEARING_DISP = "H" and
#'   marked as held; others as not held. If a CLSDATE on an unheld hearing is
#'   provided, that is used instead of HEARING_DATE. Merged appeals and dummy
#'   data are also excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_hearing(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_hearing <- function (con, join, where) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  query <- c(
    "select BFCORLID, BFDNOD as APPEAL_DATE, BFKEY, HEARING_DISP, HEARING_DATE, CLSDATE",
    "from BRIEFF",
    "join HEARSCHED on BFKEY = FOLDER_NR")

  if(!missing(join)) query %<>% c("join", join)

  query %<>% c(
    "where", EventCaseExclusions,
    "and", EventHearingExclusions
  )
  if(!missing(where)) query %<>% c("and", where)

  query %<>% paste(collapse = " ")

  print(paste("Querying VACOLS:", query))
  result <- dbGetQuery(con, query) %>%
    mutate(EVENT_TYPE = ifelse(HEARING_DISP == "H", "HEARING", "HEARING_EXCEPTION")) %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = HEARING_DATE, CLSDATE)

  result[result$EVENT_TYPE == "HEARING_EXCEPTION" & !is.na(result$CLSDATE),]$DATE <-
    result[result$EVENT_TYPE == "HEARING_EXCEPTION" & !is.na(result$CLSDATE),]$CLSDATE

  result %<>%
    select(-CLSDATE) %>%
    filter(!is.na(DATE)) %>%
    unique()

  return(result)
}


#' Retrieve a log of Transcription (TO_TRANSCRIPT and FROM_TRANSCRIPT) events matching the
#' specified criteria.
#'
#' @section Methodology: Uses the dates from the CONSENT and CONRET columns on the FOLDER
#'   table, limited to appeals where the hearing has been held (HEARING_DISP =
#'   'H'), and excluding RO formal hearings (HEARING_TYPE <> 'F'). Merged
#'   appeals and dummy data are also excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_transcript(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_transcript <- function (con, join, where) {
  source("R/constants.R")

  join <- paste0("HEARSCHED on BFKEY = FOLDER_NR",
                 ifelse(missing(join), "", paste0(" join ", join)))

  where <- paste0(
    EventHearingExclusions,
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("CONSENT", "CONRET"), c("TO_TRANSCRIPT", "FROM_TRANSCRIPT"), join = join, where = where))
}


#' Retrieve a log of Translation (TRANSLATION_REQ, TO_TRANSLATION and FROM_TRANSLATION) events
#' matching the specified criteria.
#'
#' @section Methodology: REQ uses the LOCDOUT date from events in the PRIORLOC table
#'   where the location is "14". TO and FROM use the LOCDOUT and LOCDIN dates, respectively,
#'   of events in the PRIORLOC table where the location is "18". The STAFF table is joined
#'   when querying the PRIORLOC table, and individuals (those where STITLE is
#'   not null) are replaced with the parent location (STITLE). Merged appeals
#'   and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_translation(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_translation <- function (con, where) {
  source("R/constants.R")

  where <- paste0(
    "(LOCSTTO = ", TransAbeyanceLoc,
    " or LOCSTTO = ", TransRequestLoc, ")",
    ifelse(missing(where), "", paste0(" and ", where))
  )

  locs <- event_getPriorLocs(con, where)
  result <- .parseTranslationLocs(locs)

  return(result)
}


#' Retrieve a log of Service Organization Review (TO_VSO and FROM_VSO) events
#' matching the specified criteria.
#'
#' @section Methodology: Uses the LOCDIN date from events in the PRIORLOC table
#'   where the location is not "55" and the subsequent event's location is "55"
#'   for TO_VSO, and the LOCDOUT date from events where the location is "55" and
#'   the subsequent location is not "55" for FROM_VSO. The STAFF table is joined
#'   when querying the PRIORLOC table, and individuals (those where STITLE is
#'   not null) are replaced with the parent location (STITLE). Merged appeals
#'   and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_vso(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_vso <- function (con, where) {
  locs <- event_getPriorLocs(con, where)
  result <- .parseVSOLocs(locs)

  return(result)
}


#' Retrieve a log of Assignment (ASSIGNMENT) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the date from the DEASSIGN column on the DECASS
#'   table. Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_assignment(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_assignment <- function (con, where) {
  join <- "DECASS on BFKEY = DEFOLDER"

  return(event_getDateCols(con, c("DEASSIGN"), c("ASSIGNMENT"), join = join, where = where))
}


#' Retrieve a log of Abeyance (TO_ABEYANCE and FROM_ABEYANCE) events matching
#' the specified criteria.
#'
#' @section Methodology: Uses the LOCDOUT and LOCDIN dates, respectively, from
#'   events where the location is "24" or "39". The STAFF table is joined when
#'   querying the PRIORLOC table, and individuals (those where STITLE is not
#'   null) are replaced with the parent location (STITLE). Merged appeals and
#'   dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_abeyance(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_abeyance <- function (con, where) {
  locs <- event_getPriorLocs(con, where)
  result <- .parseAbeyanceLocs(locs)

  return(result)
}


#' Retrieve a log of Outside Medical Opinion (TO_OMO and FROM_OMO) events
#' matching the specified criteria.
#'
#' @section Methodology: Uses the LOCDOUT and LOCDIN dates, respectively, from
#'   events where the location is "92" (Outside BVA) that are preceded by an
#'   event where the location is "20" (Opinion Request). The STAFF table is
#'   joined when querying the PRIORLOC table, and individuals (those where
#'   STITLE is not null) are replaced with the parent location (STITLE). Merged
#'   appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_omo(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_omo <- function (con, where) {
  locs <- event_getPriorLocs(con, where)
  result <- .parseOMOLocs(locs)

  return(result)
}


#' Retrieve a log of Decision Drafted (DRAFTED) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the date from the DERECEIVE column on the DECASS
#'   table. Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_drafted(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_drafted <- function (con, where) {
  join <- "DECASS on BFKEY = DEFOLDER"

  return(event_getDateCols(con, c("DERECEIVE"), c("DRAFTED"), join = join, where = where))
}


#' Retrieve a log of Signed Decision (SIGNED_DECISION) events matching the
#' specified criteria.
#'
#' @section Methodology: Looks for a series of events in the following set of
#'   locations: 1. the decision team (D[1-5]), 2. one or more of the
#'   administration team (A.+|SUP|OPR), and 3. Central Dispatch (30). Uses the
#'   LOCDIN date from the first event. The STAFF table is joined when querying
#'   the PRIORLOC table, and individuals (those where STITLE is not null) are
#'   replaced with the parent location (STITLE). Merged appeals and dummy data
#'   are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_decision(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_decision <- function (con, where) {
  locs <- event_getPriorLocs(con, where)
  result <- .parseDecisionLocs(locs)

  return(result)
}


#' Retrieve a log of Quality Review (QR) events matching the specified criteria.
#'
#' @section Methodology: Uses the LOCDIN date from events where the location is
#'   "48" (Quality Review). The STAFF table is joined when querying the PRIORLOC
#'   table, and individuals (those where STITLE is not null) are replaced with
#'   the parent location (STITLE). Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_qr(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_qr <- function (con, where) {
  source("R/constants.R")

  where <- paste0(
    "LOCSTTO = ", QRLoc,
    ifelse(missing(where), "", paste0(" and ", where))
  )

  locs <- event_getPriorLocs(con, where)
  result <- .parseQRLocs(locs)

  return(result)
}


#' Retrieve a log of Remand Return (REMAND_RETURN) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the LOCDOUT date from events where the location is
#'   "96" (Remand Return). If checked out to 96 multiple times, the last time is
#'   used. The STAFF table is joined when querying the PRIORLOC table, and
#'   individuals (those where STITLE is not null) are replaced with the parent
#'   location (STITLE). Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_remreturn(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_remreturn <- function (con, where) {
  source("R/constants.R")

  where <- paste0(
    "LOCSTTO = ", RemReturnLoc,
    ifelse(missing(where), "", paste0(" and ", where))
  )

  locs <- event_getPriorLocs(con, where)
  result <- .parseRemReturnLocs(locs)

  return(result)
}


#' Retrieve a log of OUTCODING (OUTCODING) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the date from the TIOCTIME column on the FOLDER
#'   table. Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_outcoding(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_outcoding <- function (con, where) {
  join <- "FOLDER on BFKEY = TICKNUM"

  return(event_getDateCols(con, c("TIOCTIME"), c("OUTCODING"), join = join, where = where))
}


#' Retrieve a log of end state events matching the specified criteria.
#'
#' Includes: Withdrawn (WITHDRAWN), Dismissed (DISMISSED), Vacated (VACATED),
#' Remand (REMAND), AOJ Grant (AOJ_GRANT), Final Dispatch (DISPATCH)
#'
#' @section Methodology: Uses the date from the BFDDEC column, classified
#'   according to the BFDC column. Consult \code{EventEndStateClassifier} in
#'   \code{constants.R} for the mapping used.  Merged appeals and dummy data are
#'   excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_endStates(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_endStates <- function (con, join, where) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  query <- c(
    "select BFCORLID, BFDNOD as APPEAL_DATE, BFKEY, BFDC, BFDDEC",
    "from BRIEFF")


  if(!missing(join)) query %<>% c("join", join)

  query %<>% c("where", EventCaseExclusions)
  if(!missing(where)) query %<>% c("and", where)

  query %<>% paste(collapse = " ")

  print(paste("Querying VACOLS:", query))
  result <- dbGetQuery(con, query) %>%
    merge(EventEndStateClassifier) %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = BFDDEC)
}


#' Retrieve a log of CAVC Decision (CAVC) events matching the specified
#' criteria.
#'
#' @section Methodology: Uses the date from the CVDDEC column, uniquing on the
#'   BFKEY column. Merged appeals and dummy data are excluded.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_cavc(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_cavc <- function (con, join, where) {
  require("magrittr")
  require("dplyr")

  join <- paste0("COVA on BFKEY = CVFOLDER",
                 ifelse(missing(join), "", paste0(" join ", join)))

  event_getDateCols(con, c("CVDDEC"), c("CAVC"), join = join, where = where) %>%
    distinct(BFKEY, DATE, .keep_all = TRUE) %>%
    return
}

# Nothing will appear if not overruled

######################################
####### COALESCED EVENT GROUPS #######
######################################

#' Retrieve a log of all events matching the specified criteria. Consult
#' individual event functions for methodology details.
#'
#' @description Includes: Notice of Disagreement (NOD), VACOLS Creation
#'   (VACOLS), Substitution (SUBSTITUTION), Assigned to Docket (DOCKET),
#'   Statement of Case (SOC), Form 9 (FORM9), Supplemental Statement of Case
#'   (SSOC), Certification to BVA (CERTIFICATION), Activated at BVA
#'   (ACTIVATION), Hearing Held (HEARING), Hearing Not Held (HEARING_EXCEPTION),
#'   Transcription (TO_TRANSCRIPT and FROM_TRANSCRIPT), Translation
#'   (TRANSLATION_REQ, TO_TRANSLATION and FROM_TRANSLATION), Service
#'   Organization Review (TO_VSO and FROM_VSO), Assignment (ASSIGNMENT),
#'   Abeyance (TO_ABEYANCE and FROM_ABEYANCE), Outside Medical Opinion (TO_OMO
#'   and FROM_OMO), Decision Drafted (DRAFT_DECISION), Decision Signed
#'   (SIGNED_DECISION), Decision Issued (DECISION), Quality Review (QR),
#'   Outcoding (OUTCODING), Withdrawn (WITHDRAWN), Dismissed (DISMISSED), Remand
#'   (REMAND), Remand Return (REMAND_RETURN), AOJ Grant (AOJ_GRANT), Final
#'   Dispatch (DISPATCH), Vacated (VACATED), CAVC Decision (CAVC)
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID},
#'   \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_all(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_all <- function (con, join, where) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  events <-
    rbind((function(con, join, where) {
      cols <- c("BFDNOD", "BFDSOC", "BFD19", "BF41STAT", "TIADTIME")
      labs <- c("NOD", "SOC", "FORM9", "CERTIFICATION", "DOCKET")

      join <- paste0(
        "FOLDER on BFKEY = TICKNUM",
        ifelse(missing(join), "", paste0(" join ", join))
      )

      where <- paste0(
        "BFAC = '1'", # only include the original action
        ifelse(missing(where), "", paste0(" and ", where))
      )

      return(event_getDateCols(con, cols, labs, join = join, where = where))
    })(con, join, where)) %>%
    rbind(event_vacolsCreation(con, join, where)) %>%
    rbind(event_substitution(con, join, where)) %>%
    rbind(event_ssoc(con, join, where)) %>%
    rbind((function(con, join, where) {
      cols <- c("TIDRECV", "TIDKTIME", "TIOCTIME")
      labs <- c("ACTIVATION", "CASE_REVIEW", "OUTCODING")

      join <- paste0(
        "FOLDER on BFKEY = TICKNUM",
        ifelse(missing(join), "", paste0(" join ", join))
      )

      return(event_getDateCols(con, cols, labs, join = join, where = where))
    })(con, join, where)) %>%
    rbind((function(con, join, where) {
      cols <- c("DEASSIGN", "DERECEIVE")
      labs <- c("ASSIGNMENT", "DRAFT_DECISION")

      join <- paste0(
        "DECASS on BFKEY = DEFOLDER",
        ifelse(missing(join), "", paste0(" join ", join))
      )

      return(event_getDateCols(con, cols, labs, join = join, where = where))
    })(con, join, where)) %>%
    rbind(event_hearing(con, join, where)) %>%
    rbind(event_transcript(con, join, where)) %>%
    rbind((function(con, join, where) {
      locs <- event_getPriorLocs(con, join, where)

      vso <- .parseVSOLocs(locs)
      translation <- .parseTranslationLocs(locs)
      abyance <- .parseAbeyanceLocs(locs)
      omos <- .parseOMOLocs(locs)
      qr <- .parseQRLocs(locs)
      decisions <- .parseDecisionLocs(locs)
      remreturn <- .parseRemReturnLocs(locs)

      return(rbind(vso, translation, abyance, omos, qr, decisions, remreturn))
    })(con, join, where)) %>%
    rbind(event_endStates(con, join, where)) %>%
    rbind(event_cavc(con, join, where)) %>%
    arrange(BFCORLID, APPEAL_DATE, DATE)

  return(events)
}
