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


#' Parse a location log to extract assignment (ASSIGNMENT) and decision (DECISION) events.
#'
#' @param loclog A complete stream of events (heuristic requires inclusion of non-pertinent events)
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{APPEAL_DATE}, \code{BFKEY}, \code{EVENT_TYPE} and \code{DATE}.
event_parseDecisionLocs <- function(loclog) {
  require("dplyr")
  require("tidyr")

  loclog <- arrange(loclog, BFCORLID, APPEAL_DATE, LOCDOUT)

  dispatchLocs <- "A.+|SUP|OPR"
  decisionLocs <- "D[1-5]"
  continueLocs <- paste("24|39|81", dispatchLocs, decisionLocs, sep = "|")

  dispatchIdxs <- sort(grep(dispatchLocs, loclog$LOC, perl = TRUE), decreasing = TRUE)
  decisionMask <- grepl(decisionLocs, loclog$LOC, perl = TRUE)
  continueMask <- grepl(continueLocs, loclog$LOC, perl = TRUE)

  result <- data.frame(
    BFCORLID = character(),
    APPEAL_DATE = .POSIXct(character()),
    BFKEY = character(),
    ASSIGNMENT = as.POSIXct(character()),
    DECISION = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )

  i <- 1
  while(i <= length(dispatchIdxs)) {
    start <- dispatchIdxs[i] - 1

    if(!decisionMask[start]) {
      i <- i + 1
      next
    }

    end <- start
    j <- start

    while(continueMask[j]) {
      j <- j - 1
      if(j == 0) { break }
      if(decisionMask[j]) { start <- j }
    }

    bfcorlid <- loclog$BFCORLID[start]
    appeal_date <- loclog$APPEAL_DATE[start]
    bfkey <- loclog$BFKEY[start]
    start_date <- loclog$LOCDOUT[start]
    end_date <- loclog$LOCDIN[end]

    result[nrow(result) + 1,] <- list(bfcorlid, appeal_date, bfkey, start_date, end_date)

    while(i <= length(dispatchIdxs) & dispatchIdxs[i] >= start) {
      i <- i + 1
    }
  }

  result <- gather(result, EVENT_TYPE, DATE, -BFCORLID, -APPEAL_DATE, -BFKEY)

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


#' Retrieve a log of all events matching the specified criteria. Includes:
#' * Notice of Disagreement (NOD)
#' * Statement of Case (SOC)
#' * Form 9 (FORM9)
#' * Supplemental Statement of Case (SSOC)
#' * Certification to BVA (CERTIFICATION)
#' * Activated at BVA (ACTIVATION)
#' * Hearing Held (HEARING)
#' * Transcript Received (TRANSCRIPT)
#' * Assignment (ASSIGNMENT)
#' * Decision (DECISION)
#' * Quality Review (QR)
#' * Withdrawn (WITHDRAWN)
#' * Dismissed (DISMISSED)
#' * Remand (REMAND)
#' * AOJ Grant (AOJGRANT)
#' * Final Dispatch (DISPATCH)
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
    # rbind((function(con, where) {
    #   cols <- c("")
    #   labs <- c("")
    #
    #   return(event_getDateCols(con, cols, labs, where = where))
    # })(con, where)) %>%
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
      decisions <- event_parseDecisionLocs(locs)
      qr <- event_parseQRLocs(locs)

      return(rbind(activations, decisions, qr))
    })(con, where)) %>%
    rbind(event_endStates(con, where)) %>%
    arrange(BFCORLID, APPEAL_DATE, DATE)

  # TODO:
  # * Translation (loc 14 and 18)
  # * CAVC
  # * Service orgs
  # * OMO
  # * Abeyance

  return(events)
}
