#' Open a connection to Caseflow. Looks for the environment variables
#' \code{cf_password}, \code{cf_host}, and \code{cf_port}.
#'
#' @param password Override the environment variables.
#' @param host Override the environment variables.
#' @param port Override the environment variables.
#' @return A \code{PostgreSQLConnection} object.
caseflowConnect <- function(password = NULL, host = NULL, port = NULL) {
  require(RPostgreSQL)

  username <- "caseflow"
  password <- if(!is.null(password)) password else Sys.getenv("cf_password")
  dbname   <- "caseflow_certification"
  host     <- if(!is.null(host)) host else Sys.getenv("cf_host")
  port     <- if(!is.null(port)) port else Sys.getenv("cf_port")

  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = host, port = port, user = username, password = password)

  return(con)
}
