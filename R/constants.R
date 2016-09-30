#' SQL \code{WHERE} statement to be globally applied to all event logs.
EventCaseExclusions <- paste(c(
  "BFKEY <> '2222222'", # omit dummy data - Joe Snuffy
  "BFKEY <> '3082477'", # omit dummy data - Jed Wagner
  "BFMPRO <> 'M'" # omit merged cases
), collapse = " and ")
