# SQL \code{WHERE} statement to be globally applied to all event logs.
EventCaseExclusions <- paste(c(
  "BFKEY <> '2222222'", # omit dummy data - Joe Snuffy
  "BFKEY <> '3082477'",  # omit dummy data - Jed Wagner
  "(BFDC is null or BFDC <> 'M')" # omit merged cases
), collapse = " and ")

# SQL \code{WHERE} statement to be applied to hearing event logs.
EventHearingExclusions <- paste(c(
  "HEARING_DISP <> 'P'", # omit postponed hearings
  "HEARING_TYPE <> 'F'" # omit RO formal hearings
), collapse = " and ")

# Dataframe for classifying the final disposition of a case.
EventEndStateClassifier <- data.frame(
  BFDC       = c('1',        '3',        '4',        '5',       '6',         '8',         '9',         'A',     'B',     'E',         'F',         'G',         'W',         'X'),
  EVENT_TYPE = c('DECISION', 'DECISION', 'DECISION', 'VACATED', 'WITHDRAWN', 'WITHDRAWN', 'WITHDRAWN', 'GRANT', 'GRANT', 'WITHDRAWN', 'WITHDRAWN', 'WITHDRAWN', 'WITHDRAWN', 'WITHDRAWN')
)

# PRIORLOC locations.
AbeyanceLocs       <- "24|39"
DecisionLocs       <- "D[1-5]"
DispatchLocs       <- "A.+|SUP|OPR"
ActivationLoc      <- "01"
CentralDispatchLoc <- "30"
OMORequestLoc      <- "20"
OutsideBVALoc      <- "92"
QRLoc              <- "48"
VSOLoc             <- "55"
TransRequestLoc    <- "14"
TransAbeyanceLoc   <- "18"
RemReturnLoc       <- "96"

# VBMS claim codes that could be End Products (EPs)
EPCandidates <- c(
  "170APPACT",
  "170APPACTPMC",
  "170PGAMC",
  "170RMD",
  "170RMDAMC",
  "170RMDPMC",
  "172GRANT",
  "172BVAG",
  "172BVAGPMC",
  "400CORRC",
  "400CORRCPMC",
  "930RC",
  "930RCPMC"
)
