#' Open a connection to VACOLS. Looks for the environment variables \code{vacols_db_password}
#' or \code{vacols_dev_password}, depending on the environment specified by \code{dev}.
#'
#' @param dev If \code{FALSE} (the default), uses the VACOLS production environment.
#'   If \code{TRUE}, uses the development environment.
#' @param password Override the environment variables.
#' @return An \code{OraConnection} object.
vacolsConnect <- function(dev = FALSE, password = NULL) {
  require("ROracle")

  username <- "DSUSER"
  password <- if(!is.null(password)) password
              else if(dev) Sys.getenv("vacols_dev_password") else Sys.getenv("vacols_db_password")
  sid      <- "BVAP"
  host     <- if(dev) "vacols.dev.vaco.va.gov" else "vacols.vaco.va.gov"
  port     <- 1526
  connect.string <- paste0(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
    "(CONNECT_DATA=(SID=", sid, ")))"
  )

  drv <- dbDriver("Oracle")
  con <- dbConnect(drv, username, password, dbname = connect.string)

  return(con)
}
