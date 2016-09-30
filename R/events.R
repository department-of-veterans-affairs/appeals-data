#' Retrieve a log of events that appear in VACOLS as date columns.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param cols Vector of column names to include in results.
#' @param labs (optional) Vector of labels to be used for the \code{event_type} variable.
#' @param join (optional) SQL statement to join a table other than BRIEFF.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{BFKEY}, \code{event_type} and \code{date}.
#'
#' @examples
#' cols <- c("BFNOD", "BFDSOC", "BFD19")
#' labs <- c("NOD", "SOC", "FORM9")
#' event_getDateCols(cols, labs, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
#'
#' event_getDateCols(c("HEARING_DATE"), c("hearing"), join = "HEARSCHED on FOLDER_NR = BFKEY", where = "HEARING_DATE > date '2016-10-01'")
event_getDateCols <- function(con, cols, labs, join, where) {
  if(missing(labs)) labs <- cols
  source("R/constants.R")

  require("magrittr")
  require("dplyr")
  require("tidyr")

  aliases <- sapply(seq_along(cols), function(i) { return(paste(cols[i], "as", labs[i])) })
  nullTests <- sapply(cols, function(x) { return(paste(x, "is not null")) })

  query <- c(
    "select BFCORLID, BFKEY,", paste(aliases, collapse = ", "),
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
    gather(event_type, date, -BFCORLID, -BFKEY) %>%
    filter(!is.na(date))

  return (result)
}


#' Retrieve a log of VACOLS Creation (VACOLS) events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{BFKEY}, \code{event_type} and \code{date}.
#' @examples
#' event_vacolsCreation(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_vacolsCreation <- function (con, where) {
  join <- "(select LOCKEY, min(LOCDOUT) CREATED from PRIORLOC group by LOCKEY) on LOCKEY = BFKEY"

  return(event_getDateCols(con, c("CREATED"), c("VACOLS"), join = join, where = where))
}


#' Retrieve a log of Notice of Disagreement dates matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{BFKEY}, \code{event_type} and \code{date}.
#' @examples
#' event_nod(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_nod <- function (con, where) {
  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("BFDNOD"), c("NOD"), where = where))
}


#' Retrieve a log of Statement of Case events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{BFKEY}, \code{event_type} and \code{date}.
#' @examples
#' event_soc(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_soc <- function (con, where) {
  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("BFDSOC"), c("SOC"), where = where))
}


#' Retrieve a log of Form 9 () events matching the specified criteria.
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{BFKEY}, \code{event_type} and \code{date}.
#' @examples
#' event_form9(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_form9 <- function (con, where) {
  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, c("BFD19"), c("FORM9"), where = where))
}


#' Retrieve a log of all events prior to certification matching the specified criteria. Includes:
#' * Notice of Disagreement (NOD)
#' * Statement of Case (SOC)
#' * Form 9 (FORM9)
#' * Supplemental Statement of Case (SSOC) ***TODO***
#'
#' @param con \code{OraConnection} to VACOLS created by \code{vacolsConnect}.
#' @param where (optional) SQL statement to filter results.
#' @return A dataframe of events, with the variables \code{BFCORLID}, \code{BFKEY}, \code{event_type} and \code{date}.
#' @examples
#' event_form9(con, where = "BFDNOD > date '2009-01-01' and BFDNOD < date '2009-12-31'")
event_all_advance <- function (con, where) {
  cols <- c("BFDNOD", "BFDSOC", "BFD19")
  labs <- c("NOD", "SOC", "FORM9")

  where <- paste0(
    "BFAC = '1'", # only include the original action
    ifelse(missing(where), "", paste0(" and ", where))
  )

  return(event_getDateCols(con, cols, labs, where = where))
}
