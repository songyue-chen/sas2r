# Model requests that were ended from outside at the same elapsed time.
#
# A request can be closed by something other than its own timeout: a proxy,
# VPN or firewall that drops a connection after a fixed time, or a time limit
# at the provider. Each such failure is recorded as an ordinary connection or
# server error, so a run can lose every long request while its records show
# only unrelated-looking failures. What gives the limit away is that the
# failures end at the same elapsed time. Random network trouble does not.

# Classes a request ended from outside is recorded under. A timeout class is
# not one of them: that is the package's own limit, reported as such.
REQUEST_CUT_ERROR_CLASSES <- c(
  "sas2r_llm_transport_error", "sas2r_llm_network_error"
)

# A failure in the first seconds is a connection that never got going (refused,
# unresolved host, an immediate server error). A cut needs a wait before it.
REQUEST_CUT_MIN_SECONDS <- 30

# How far apart two failures may end and still count as the same elapsed
# time. Connection setup and upload vary by a second or two between requests.
REQUEST_CUT_WINDOW_SECONDS <- 3

# A failure this close to the request's configured limit is that limit.
REQUEST_CUT_LIMIT_MARGIN_SECONDS <- 5

#' Find the largest group of failed requests that ended at the same elapsed time
#'
#' @param completed Completed-request ledger records, one per request.
#' @return `NULL` when fewer than two failures share an elapsed time, else a
#'   list with the number of requests, the shortest and longest elapsed time in
#'   seconds, and the configured request limit in seconds (`NA` when unknown).
#' @noRd
usage_request_cuts <- function(completed) {
  failed <- Filter(function(record) {
    identical(record$status, "failed") &&
      isTRUE(record$error_class %in% REQUEST_CUT_ERROR_CLASSES)
  }, completed)
  seconds <- vapply(failed, function(record) {
    scalar_number_or_na(record$duration_ms) / 1000
  }, numeric(1))
  limits <- vapply(failed, function(record) {
    scalar_number_or_na(record$timeout_seconds)
  }, numeric(1))
  waited <- !is.na(seconds) & seconds >= REQUEST_CUT_MIN_SECONDS &
    (is.na(limits) | seconds < limits - REQUEST_CUT_LIMIT_MARGIN_SECONDS)
  seconds <- seconds[waited]
  limits <- limits[waited]
  if (length(seconds) < 2L) return(NULL)

  group_from <- function(start) {
    seconds >= start & seconds <= start + REQUEST_CUT_WINDOW_SECONDS
  }
  sizes <- vapply(seconds, function(start) sum(group_from(start)), integer(1))
  if (max(sizes) < 2L) return(NULL)
  group <- group_from(seconds[[which.max(sizes)]])
  known_limits <- limits[group][!is.na(limits[group])]
  list(
    requests = sum(group),
    shortest_seconds = round(min(seconds[group]), 1),
    longest_seconds = round(max(seconds[group]), 1),
    timeout_seconds = if (length(known_limits)) max(known_limits) else NA_real_
  )
}

#' Describe a cut for the run summary, or return nothing when there is none
#'
#' Reads the `request_cuts` entry of a usage summary, either as built in memory
#' or as read back from `report.json`.
#' @noRd
migration_request_cut_lines <- function(usage) {
  cuts <- usage$request_cuts
  requests <- scalar_number_or_na(cuts$requests)
  shortest <- scalar_number_or_na(cuts$shortest_seconds)
  longest <- scalar_number_or_na(cuts$longest_seconds)
  if (is.na(requests) || is.na(shortest) || is.na(longest)) return(character())
  limit <- scalar_number_or_na(cuts$timeout_seconds)
  elapsed <- if (identical(shortest, longest)) {
    sprintf("about %.1f seconds", shortest)
  } else {
    sprintf("%.1f to %.1f seconds", shortest, longest)
  }
  c(
    paste0(
      sprintf("Requests cut short: %d model requests ended in a connection or server error after %s each",
              as.integer(requests), elapsed),
      if (is.na(limit)) "." else sprintf(
        ", well short of the %s-second limit set for a request.",
        format(limit, scientific = FALSE, trim = TRUE)
      )
    ),
    paste(
      "Failures at the same elapsed time point to a time limit outside sas2r:",
      "a proxy, VPN or firewall on the network, or the provider.",
      "A request that needs longer than this cannot finish, and retrying it unchanged will not help."
    )
  )
}
