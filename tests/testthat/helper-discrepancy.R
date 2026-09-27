discrepancy_fixture <- function() {
  # Synthetic comparator output; no user data. Values deliberately recognizable
  # so accidental forwarding of detail rows fails the request-boundary tests.
  base <- data.frame(id = paste0("RECORD_SENTINEL_", 1:3), day = 101:103)
  comp <- transform(base, day = day + 3653)
  cmp <- compare_datasets_aligned(base, comp, keys = "id")
  list(kind = "dataset", target_key = "work.result", status = "failed", has_reference = TRUE,
    reference_path = "/REFERENCE_PATH_SENTINEL/data.rds",
    checks = list(reference_comparison = list(summary = cmp$summary)),
    differences = list(digest = unclass(diff_digest(cmp)), structure = cmp$structure,
      mismatches = cmp$details))
}
