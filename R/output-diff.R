# Normalized cell difference summaries and comparison report utilities


#' @export
print.sas2r_output_comparison_report <- function(x, ...) {
  print.sas2r_comparison_report(x, ...)
}

#' @export
as.data.frame.sas2r_output_comparison_report <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame.sas2r_comparison_report(x, row.names = row.names, optional = optional, ...)
}
