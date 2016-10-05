#' SQL \code{WHERE} statement to be globally applied to all event logs.
EventCaseExclusions <- paste(c(
  "BFKEY <> '2222222'", # omit dummy data - Joe Snuffy
  "BFKEY <> '3082477'", # omit dummy data - Jed Wagner
  "BFDC <> 'M'" # omit merged cases
), collapse = " and ")

#' SQL \code{WHERE} statement to be applied to hearing event logs.
EventHearingExclusions <- paste(c(
  "HEARING_DISP = 'H'", # limit to held hearings
  "HEARING_TYPE <> 'F'" # omit RO formal hearings
), collapse = " and ")

#' Dataframe for classifying the final disposition of a case.
EventEndStateClassifier <- data.frame(
  BFDC       = c('1',        '3',      '4',        '5',       '6',         '8',         '9',         'A',         'B',         'E',         'F',         'G',         'W'),
  EVENT_TYPE = c('DISPATCH', 'REMAND', 'DISPATCH', 'VACATED', 'DISMISSED', 'DISMISSED', 'WITHDRAWN', 'AOJ_GRANT', 'AOJ_GRANT', 'WITHDRAWN', 'WITHDRAWN', 'WITHDRAWN', 'WITHDRAWN')
)

#' PRIORLOC locations.
AbeyanceLocs       <- "24|39"
DecisionLocs       <- "D[1-5]"
DispatchLocs       <- "A.+|SUP|OPR"
CentralDispatchLoc <- "30"
OMORequestLoc      <- "20"
OutsideBVALoc      <- "92"
VSOLoc             <- "55"
