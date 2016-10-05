#' Retrieve a log of events that appear in VACOLS as date columns.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param cols Vector of column names to include in results.
#' @param labs (optional) Vector of labels to be used for the \code{EVENT_TYPE} variable.
#' @param join (optional) SQL statement to join a table other than BRIEFF.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
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


#' Retrieve a log of locations from the VACOLS PRIORLOC table.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{LOCDOUT}, \code{LOCDIN},
#' \code{LOCSTTO}, \code{LOCSTOUT}, \code{LOCSTRCV}, \code{LOC} and \code{LOC_NAME}.#'
#' @examples
#' event_getPriorLocs(con, "LOCSTTO = '48' and LOCSTOUT > date '2015-01-01' and LOCSTOUT < date '2015-12-31'")
event_getPriorLocs <- function(con, where) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  query <- c(
    "select BFCORLID, BFDNOD as APPEAL_DATE, BFKEY, LOCDOUT, LOCDIN, LOCSTTO, LOCSTOUT, LOCSTRCV",
    "from BRIEFF",
    "join PRIORLOC on LOCKEY = BFKEY",
    "where", EventCaseExclusions
  )
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


#' Parse a location log to extract Service Organization (TO_VSO and FROM_VSO) events.
#'
#' @param loclog A stream of events
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
event_parseVSOLocs <- function(loclog) {
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


#' Parse a location log to extract Abeyance (TO_ABEYANCE and FROM_ABEYANCE) events.
#'
#' @param loclog A stream of events
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
event_parseAbeyanceLocs <- function(loclog) {
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


#' Parse a location log to extract assignment (ASSIGNMENT) events.
#'
#' @param loclog A stream of events
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
event_parseAssignmentLocs <- function(loclog) {
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

  result <- loclog %>%
    filter(
      grepl(DecisionLocs, LOC, perl = TRUE),
      grepl(DecisionLocs, LOC_PLUS1, perl = TRUE)
    ) %>%
    group_by(BFKEY) %>%
    slice(which.min(LOCDOUT)) %>%
    ungroup() %>%
    mutate(EVENT_TYPE = "ASSIGNMENT") %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = LOCDIN)

  return(result)
}


#' Parse a location log to extract Outside Medical Opinion (TO_OMO and FROM_OMO) events.
#'
#' @param loclog A stream of events
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
event_parseOMOLocs <- function(loclog) {
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


#' Parse a location log to extract decision (DECISION) events.
#'
#' @param loclog A stream of events
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
event_parseDecisionLocs <- function(loclog) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  loclog %<>%
    arrange(BFCORLID, APPEAL_DATE, LOCDOUT) %>%
    mutate(LOC_PLUS1 = ifelse(
      loclog$BFCORLID == c(tail(loclog$BFCORLID, -1), rep(NA, 1)) &
        loclog$APPEAL_DATE == c(tail(loclog$APPEAL_DATE, -1), rep(NA, 1)),
      c(tail(loclog$LOC, -1), rep(NA, 1)), NA
    )) %>%
    mutate(LOC_PLUS2 = ifelse(
      loclog$BFCORLID == c(tail(loclog$BFCORLID, -2), rep(NA, 2)) &
        loclog$APPEAL_DATE == c(tail(loclog$APPEAL_DATE, -2), rep(NA, 2)),
      c(tail(loclog$LOC, -2), rep(NA, 2)), NA
    ))

  result <- loclog %>%
    filter(
      grepl(DecisionLocs, LOC, perl = TRUE),
      grepl(DispatchLocs, LOC_PLUS1, perl = TRUE),
      LOC_PLUS2 == CentralDispatchLoc
    ) %>%
    mutate(EVENT_TYPE = "DECISION") %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = LOCDIN)

  return(result)
}


#' Parse a location log to extract Quality Review (QR) events.
#'
#' @param loclog A stream of events
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
event_parseQRLocs <- function(loclog) {
  require("magrittr")
  require("dplyr")

  loclog %>%
    filter(LOC == '48') %>%
    mutate(EVENT_TYPE = "QR") %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = LOCDIN) %>%
    return
}


#' Parse a location log to extract Activated at BVA (ACTIVATION) events.
#'
#' @param loclog A stream of events
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
event_parseActivationLocs <- function(loclog) {
  require("magrittr")
  require("dplyr")

  loclog %>%
    filter(LOC == '01') %>%
    mutate(EVENT_TYPE = "ACTIVATION") %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = LOCDOUT) %>%
    return
}


#' Retrieve a log of Notice of Disagreement (NOD) dates matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_nod(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_nod <- function (con, where) {
  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("BFDNOD"), c("NOD"), where = where))
}


#' Retrieve a log of VACOLS Creation (VACOLS) events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_vacolsCreation(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_vacolsCreation <- function (con, where) {
  join <- "(select LOCKEY, min(LOCDOUT) VACOLS from PRIORLOC group by LOCKEY) on LOCKEY = BFKEY"

  return(event_getDateCols(con, c("VACOLS"), join = join, where = where))
}


#' Retrieve a log of Statement of Case (SOC) events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
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
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_form9(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_form9 <- function (con, where) {
  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("BFD19"), c("FORM9"), where = where))
}


#' Retrieve a log of Supplemental Statement of Case (SSOC) events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_ssoc(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_ssoc <- function (con, where) {
  require("magrittr")
  require("dplyr")

  cols <- c("BFSSOC1", "BFSSOC2", "BFSSOC3", "BFSSOC4", "BFSSOC5")

  result <- event_getDateCols(con, cols, where = where) %>%
    mutate(EVENT_TYPE = 'SSOC') %>%
    distinct(BFCORLID, DATE, .keep_all = TRUE)

  return(result)
}


#' Retrieve a log of Certification to BVA (CERTIFICATION) events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_certification(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_certification <- function (con, where) {
  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("BF41STAT"), c("CERTIFICATION"), where = where))
}


#' Retrieve a log of Assigned to Docket (DOCKET) events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_docket(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_docket <- function (con, where) {
  join <- "FOLDER on BFKEY = TICKNUM"

  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("TIADTIME"), c("ACTIVATION"), join = join, where = where))
}


#' Retrieve a log of Hearing Held (HEARING) events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_hearing(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_hearing <- function (con, where) {
  source("R/constants.R")

  join <- "HEARSCHED on BFKEY = FOLDER_NR"

  where <- paste0(
    EventHearingExclusions,
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("HEARING_DATE"), c("HEARING"), join = join, where = where))
}


#' Retrieve a log of Transcript Received (TRANSCRIPT) events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_transcript(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_transcript <- function (con, where) {
  source("R/constants.R")

  join <- "HEARSCHED on BFKEY = FOLDER_NR"

  where <- paste0(
    EventHearingExclusions,
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("CONRET"), c("TRANSCRIPT"), join = join, where = where))
}


#' Retrieve a log of end state events matching the specified criteria. Includes:
#' * Withdrawn (WITHDRAWN)
#' * Dismissed (DISMISSED)
#' * Remand (REMAND)
#' * AOJ Grant (AOJGRANT)
#' * Final Dispatch (DISPATCH)
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_endStates(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_endStates <- function (con, where) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  query <- c(
    "select BFCORLID, BFDNOD as APPEAL_DATE, BFKEY, BFDC, BFDDEC",
    "from BRIEFF",
    "where", EventCaseExclusions
  )
  if(!missing(where)) query %<>% c("and", where)
  query %<>% paste(collapse = " ")

  print(paste("Querying VACOLS:", query))
  result <- dbGetQuery(con, query) %>%
    merge(EventEndStateClassifier) %>%
    select(BFCORLID, APPEAL_DATE, BFKEY, EVENT_TYPE, DATE = BFDDEC)
}


#' Retrieve a log of CAVC Decision (CAVC) events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_cavc(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_cavc <- function (con, where) {
  require("magrittr")
  require("dplyr")

  join <- "COVA on BFKEY = CVFOLDER"

  event_getDateCols(con, c("CVDDEC"), c("CAVC"), join = join, where = where) %>%
    distinct(BFKEY, DATE, .keep_all = TRUE) %>%
    return
}


#' Retrieve a log of all events matching the specified criteria. Includes:
#' * Notice of Disagreement (NOD)
#' * Statement of Case (SOC)
#' * Form 9 (FORM9)
#' * Supplemental Statement of Case (SSOC)
#' * Certification to BVA (CERTIFICATION)
#' * Activated at BVA (ACTIVATION)
#' * Hearing Held (HEARING)
#' * Transcript Received (TRANSCRIPT)
#' * Service Organization (TO_VSO and FROM_VSO)
#' * Assignment (ASSIGNMENT)
#' * Abeyance (TO_ABEYANCE and FROM_ABEYANCE)
#' * Outside Medical Opinion (TO_OMO and FROM_OMO)
#' * Decision (DECISION)
#' * Quality Review (QR)
#' * Withdrawn (WITHDRAWN)
#' * Dismissed (DISMISSED)
#' * Remand (REMAND)
#' * AOJ Grant (AOJGRANT)
#' * Final Dispatch (DISPATCH)
#' * CAVC Decision (CAVC)
#'
#' Does not include:
#' * VACOLS Creation (VACOLS)
#' * Assigned to Docket (DOCKET)
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
#' @examples
#' event_all(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_all <- function (con, where) {
  source("R/constants.R")

  require("magrittr")
  require("dplyr")

  events <-
    rbind((function(con, where) {
      cols <- c("BFDNOD", "BFDSOC", "BFD19", "BF41STAT")
      labs <- c("NOD", "SOC", "FORM9", "CERTIFICATION")

      join <- "FOLDER on BFKEY = TICKNUM"

      where <- paste0(
        "BFAC = '1'", # only include the original action
        ifelse(missing(where), "", paste0(" and ", where))
      )

      return(event_getDateCols(con, cols, labs, join = join, where = where))
    })(con, where)) %>%
    rbind(event_ssoc(con, where)) %>%
    rbind((function(con, where) {
      cols <- c("HEARING_DATE", "CONRET")
      labs <- c("HEARING", "TRANSCRIPT")

      join <- "HEARSCHED on BFKEY = FOLDER_NR"

      where <- paste0(
        EventHearingExclusions,
        ifelse(missing(where), "", paste0(" and ", where))
      )

      return(event_getDateCols(con, cols, labs, join = join, where = where))
    })(con, where)) %>%
    rbind((function(con, where) {
      locs <- event_getPriorLocs(con, where)

      activations <- event_parseActivationLocs(locs)
      vso <- event_parseVSOLocs(locs)
      assignments <- event_parseAssignmentLocs(locs)
      abyance <- event_parseAbeyanceLocs(locs)
      omos <- event_parseOMOLocs(locs)
      decisions <- event_parseDecisionLocs(locs)
      qr <- event_parseQRLocs(locs)

      return(rbind(activations, vso, assignments, abyance, omos, decisions, qr))
    })(con, where)) %>%
    rbind(event_endStates(con, where)) %>%
    rbind(event_cavc(con, where)) %>%
    arrange(BFCORLID, APPEAL_DATE, DATE)

  return(events)
}
