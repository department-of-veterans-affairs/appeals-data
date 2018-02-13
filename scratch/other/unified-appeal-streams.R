# Unify appeal streams for a Veteran

# if BFAC is '1', this is a root
# if BFKEY ends in 'B' or 'W', look up parent by BFKEY
# else look to dates
# if prior decision date is null, abort
# if decision date <= prior decision date, abort (prevents loops)
# if multiple candidates for parent, look to issues
# create sets of issue categories present on each appeal as defined by merging guidance
# create set of categories present in more than one candidate set, abort if current appeal intersects with this set
# match if current appeal intersects with exactly one candidate set
# otherwise, abort

appeals <- query("
  select BFCORLID, BFKEY, BFAC, BFDC, BFDDEC, BFDPDCN
  from BRIEFF
  join (
    select distinct BFCORLID U_BFCORLID
    from BRIEFF
    where BFMPRO <> 'HIS'
      and BFCORLID like '%01_'
  ) on BFCORLID = U_BFCORLID
")

issues <- query("
  select ISSKEY, ISSPROG, ISSCODE, ISSLEV1, ISSLEV2
  from ISSUES
  join BRIEFF on ISSKEY = BFKEY
  join (
    select distinct BFCORLID U_BFCORLID
    from BRIEFF
    where BFMPRO <> 'HIS'
      and BFCORLID like '%01_'
  ) on BFCORLID = U_BFCORLID
")

issues$categories <- paste(issues$ISSPROG, issues$ISSCODE, sep = "-")

vets <- appeals %>%
  group_by(BFCORLID) %>%
  summarize(n_streams = n(), n_children = sum(BFAC %in% as.character(2:9))) %>%
  filter(n_streams > 1, n_children > 0)

appeals$parent = NA
vets$match_remclose <- 0
vets$match_priordec <- 0
vets$match_issues <- 0
vets$error_missingremclose <- 0
vets$error_nopriordec <- 0
vets$error_invalidpriordec <- 0
vets$error_missingpriordec <- 0
vets$error_multipleissuecandidates <- 0
vets$error_noissuecandidate <- 0

for (i in 1:nrow(vets)) {
  .bfcorlid <- vets$BFCORLID[i]
  .appeals <- filter(appeals, BFCORLID == .bfcorlid)

  .child_appeals <- filter(.appeals, BFAC %in% as.character(2:9))
  .child_appeals$match_remclose <- FALSE
  .child_appeals$match_priordec <- FALSE
  .child_appeals$match_issues <- FALSE
  .child_appeals$error_missingremclose <- FALSE
  .child_appeals$error_nopriordec <- FALSE
  .child_appeals$error_invalidpriordec <- FALSE
  .child_appeals$error_missingpriordec <- FALSE
  .child_appeals$error_multipleissuecandidates <- FALSE
  .child_appeals$error_noissuecandidate <- FALSE

  for (j in 1:nrow(.child_appeals)) {
    .appeal <- .child_appeals[j,]

    if (substr(.appeal$BFKEY, nchar(.appeal$BFKEY), nchar(.appeal$BFKEY)) %in% c("B", "W")) {
      .bfkey = substr(.appeal$BFKEY, 1, nchar(.appeal$BFKEY) - 1)
      if (.bfkey %in% .appeals$BFKEY) {
        appeals$parent[appeals$BFKEY == .appeal$BFKEY] <- .bfkey
        .child_appeals$match_remclose[j] <- TRUE
      } else {
        .child_appeals$error_missingremclose[j] <- TRUE
      }

      next
    }

    if (is.na(.appeal$BFDPDCN)) {
      .child_appeals$error_nopriordec[j] <- TRUE
      next
    }

    if (!is.na(.appeal$BFDDEC) & .appeal$BFDPDCN >= .appeal$BFDDEC) {
      .child_appeals$error_invalidpriordec[j] <- TRUE
      next
    }

    .issue_candidates <- filter(.appeals, BFDDEC == .appeal$BFDPDCN)

    if (nrow(.issue_candidates) == 0) {
      .child_appeals$error_missingpriordec[j] <- TRUE
      next
    }

    if (nrow(.issue_candidates) == 1) {
      appeals$parent[appeals$BFKEY == .appeal$BFKEY] <- .issue_candidates$BFKEY[1]
      .child_appeals$match_priordec[j] <- TRUE
      next
    }

    .appeal_issue_categories <- filter(issues, ISSKEY == .appeal$BFKEY)$categories %>% unique()
    .issue_matches <- character()

    for (k in 1:nrow(.issue_candidates)) {
      .candidate <- .issue_candidates$BFKEY[k]
      .candidate_issue_categories <- filter(issues, ISSKEY == .candidate)$categories %>% unique()
      .intersect <- intersect(.appeal_issue_categories, .candidate_issue_categories)
      if (length(.intersect) > 0) { .issue_matches <- c(.issue_matches, .candidate)}
    }

    if (length(.issue_matches) > 1) {
      .child_appeals$error_multipleissuecandidates[j] <- TRUE
    } else if (length(.issue_matches) == 1) {
      appeals$parent[appeals$BFKEY == .appeal$BFKEY] <- .issue_matches
      .child_appeals$match_issues[j] <- TRUE
    } else {
      .child_appeals$error_noissuecandidate[j] <- TRUE
    }
  }

  vets$match_remclose[i] <- sum(.child_appeals$match_remclose)
  vets$match_priordec[i] <- sum(.child_appeals$match_priordec)
  vets$match_issues[i] <- sum(.child_appeals$match_issues)
  vets$error_missingremclose[i] <- sum(.child_appeals$error_missingremclose)
  vets$error_nopriordec[i] <- sum(.child_appeals$error_nopriordec)
  vets$error_invalidpriordec[i] <- sum(.child_appeals$error_invalidpriordec)
  vets$error_missingpriordec[i] <- sum(.child_appeals$error_missingpriordec)
  vets$error_multipleissuecandidates[i] <- sum(.child_appeals$error_multipleissuecandidates)
  vets$error_noissuecandidate[i] <- sum(.child_appeals$error_noissuecandidate)
}
