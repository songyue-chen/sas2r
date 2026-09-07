# Runtime helpers: generated-code plumbing. Part of the runtime every
# translated program carries; see ?sas2r_runtime.

# Internal: split a SAS two-level name ("lib.member"; a bare name means work)
# after substituting &macro variables. Generated code, not users, calls this.
split_ds <- function(ds, macro_vars = character()) {
  if (length(macro_vars) > 0L && is.character(ds) && length(ds) == 1L) {
    for (nm in names(macro_vars)) {
      if (nzchar(nm)) {
        ds <- gsub(paste0("&", nm, "\\b"), macro_vars[[nm]], ds)
      }
    }
  }
  p <- strsplit(ds, ".", fixed = TRUE)[[1]]
  if (length(p) == 1L) c(lib = "work", member = p[1])
  else c(lib = p[1], member = p[2])
}
