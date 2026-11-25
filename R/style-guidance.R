# One rendered statement of coding style for the roles that write R. The
# reviewer judges semantics only and never receives it. Preference never
# overrides faithfulness: a package is used when its functions express the SAS
# behavior, and base R or a bundle helper when they do not.
STYLE_PACKAGE_ROLES <- c(
  dplyr = "DATA step row logic, IF/WHERE filters, derived variables, BY-group work after sas_sort, PROC SQL joins and summaries",
  tidyr = "PROC TRANSPOSE and other reshaping",
  ggplot2 = "figures from SGPLOT, SGRENDER and GTL templates, drawing source-supplied statistics rather than recomputing them",
  stringr = "character functions such as SUBSTR, SCAN, INDEX, COMPRESS and TRANWRD",
  forcats = "format-driven level order and recoding of categorical variables",
  lubridate = "SAS date, time and datetime arithmetic and intervals",
  purrr = "repeating one operation over several datasets or parameter sets",
  tibble = "small lookup and reference tables built in code"
)

# Opt-in only: named when the configuration asks for the pharmaverse dialect,
# and then only for packages that are allowlisted and installed. The dialect
# adds no package to the allowlist. These packages carry their own conventions
# (imputation, flags, denominators), so the wording below makes the SAS program
# the specification and a package function a stand-in for it, never the reverse.
STYLE_PHARMAVERSE_ROLES <- c(
  admiral = "ADaM derivation steps: merged and looked-up variables, date and datetime variables, flags, baseline and change, derived parameters",
  tern = "analysis functions for clinical summary tables",
  rtables = "layouts for clinical summary tables",
  gtsummary = "summary tables of counts and descriptive statistics"
)

pharmaverse_style_lines <- function(facts, installed) {
  candidates <- intersect(names(STYLE_PHARMAVERSE_ROLES), facts$allowed)
  usable <- intersect(candidates, installed)
  absent <- setdiff(candidates, installed)
  c(
    paste("Pharmaverse packages: the SAS program is the specification. A package function may stand in for",
      "hand-written steps only where it reproduces that program's own logic exactly."),
    if (length(usable)) c(
      "These allowlisted, installed packages may be used on that condition:",
      paste0("- ", usable, "::* for ", STYLE_PHARMAVERSE_ROLES[usable])
    ) else "No pharmaverse package is both allowlisted and installed here, so use none.",
    if (length(absent)) paste0("Allowlisted but not installed here, so do not use: ",
      paste(absent, collapse = ", "), "."),
    if (length(usable)) c(
      paste("For every such call, set each argument that affects results explicitly (imputation, reference dates,",
        "ordering and ties, missing values, rounding, denominators), so the code can be checked against the SAS",
        "source without reading package defaults."),
      paste("If a function's rules cannot be made equal to the SAS code's, write the step with dplyr or base R and",
        "the bundle helpers; that choice is correct, not a defect."),
      paste("Produce exactly the datasets, variables, records, labels and order the SAS program produces.",
        "Add nothing a package adds by convention, and drop nothing."),
      "A package function never replaces a bundle helper named above.")
  )
}

render_style_guidance <- function(config = list()) {
  dialect <- config$dialect %||% "tidyverse"
  if (identical(dialect, "base")) {
    return(paste(c(
      "Style: base R first, faithful always.",
      paste("Prefer base R and the bundle helpers. Use an allowlisted package only where base R",
        "cannot express the SAS behavior faithfully or clearly; that choice is correct, not a defect.")),
      collapse = "\n"))
  }
  pharmaverse <- is.character(dialect) && length(dialect) == 1L && !is.na(dialect) &&
    identical(tolower(dialect), "pharmaverse")
  if (!identical(dialect, "tidyverse") && !pharmaverse) return(paste("Style:", dialect))
  facts <- agent_package_facts(config$allowlist)
  installed <- names(facts$versions)[facts$versions != "unknown"]
  candidates <- intersect(names(STYLE_PACKAGE_ROLES), facts$allowed)
  preferred <- intersect(candidates, installed)
  absent <- setdiff(candidates, installed)
  paste(c(
    if (pharmaverse) "Style: tidyverse first, pharmaverse only where it is exact, faithful always." else
      "Style: tidyverse first, faithful always.",
    "Prefer readable |> pipelines with dplyr::mutate, filter, select, rename, group_by and summarise for ordinary transformations. Avoid repeated column assignment and row indexing when an equivalent pipeline is clearer. Keep base R for behavior that needs it; explain non-obvious choices. Repairs must not restyle unrelated code.",
    if (length(preferred)) c(
      "Prefer these allowlisted, installed packages whenever their functions express the SAS behavior faithfully:",
      paste0("- ", preferred, "::* for ", STYLE_PACKAGE_ROLES[preferred])),
    if (length(absent)) paste0("Allowlisted but not installed here, so do not use: ",
      paste(absent, collapse = ", "), "."),
    paste("When no preferred package expresses the behavior faithfully, use base R or the bundle helpers;",
      "that choice is correct, not a defect. Never change SAS behavior to fit a package idiom."),
    paste("Bundle helpers stay mandatory where they carry SAS semantics: lib_read/lib_write for data access,",
      "sas_sort for order (SAS missing sorts first; dplyr::arrange puts NA last), sas_merge for MERGE/BY,",
      "chr_cmp and %notin% for comparisons that may involve missing values, sas_round and the format helpers for ROUND and PUT."),
    if (pharmaverse) pharmaverse_style_lines(facts, installed)),
    collapse = "\n")
}

# Advisory counts for comparing live runs: calls into the preferred packages
# against base data-frame operations. They never gate or grade anything.
STYLE_TIDYVERSE_PACKAGES <- c("dplyr", "tidyr", "ggplot2", "stringr", "forcats", "purrr", "lubridate", "tibble")
STYLE_BASE_DATA_OPERATIONS <- c("[", "merge", "subset", "aggregate", "order", "transform",
  "tapply", "split", "rbind", "cbind", "do.call")

code_style_counts <- function(code) {
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(list(tidyverse = 0L, base = 0L))
  tidy <- 0L
  base <- 0L
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- e[[1L]]
    if (is.call(fn) && length(fn) == 3L && as.character(fn[[1L]])[1L] %in% c("::", ":::")) {
      if (as.character(fn[[2L]])[1L] %in% STYLE_TIDYVERSE_PACKAGES) tidy <<- tidy + 1L
    } else if (is.name(fn) && as.character(fn) %in% STYLE_BASE_DATA_OPERATIONS) {
      base <<- base + 1L
    }
    for (i in seq_along(e)) {
      if (identical(e[[i]], quote(expr = ))) next
      walk(e[[i]])
    }
  }
  for (e in exprs) walk(e)
  list(tidyverse = tidy, base = base)
}

# Calls into the pharmaverse packages, counted the same advisory way. Kept
# apart from code_style_counts() so an ordinary run's observation is unchanged.
code_pharmaverse_calls <- function(code) {
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(0L)
  names <- unlist(lapply(exprs, all.names), use.names = FALSE)
  at <- which(names %in% c("::", ":::"))
  at <- at[at < length(names)]
  sum(names[at + 1L] %in% names(STYLE_PHARMAVERSE_ROLES))
}

migration_style_observation <- function(selected_revisions) {
  code <- lapply(selected_revisions, revision_code)
  counts <- lapply(code, code_style_counts)
  pharmaverse <- sum(vapply(code, code_pharmaverse_calls, integer(1)))
  c(list(tidyverse = sum(vapply(counts, `[[`, integer(1), "tidyverse")),
         base = sum(vapply(counts, `[[`, integer(1), "base")),
         components = length(counts)),
    # Present only when such calls exist, which needs the opt-in dialect.
    if (pharmaverse > 0L) list(pharmaverse = pharmaverse))
}

migration_style_line <- function(observation) {
  if (is.null(observation)) return(character())
  sprintf("Style observation (advisory): tidyverse package calls %d; base data-frame operations %d%s; across %d components.",
    observation$tidyverse, observation$base,
    if (length(observation$pharmaverse)) sprintf("; pharmaverse package calls %d", as.integer(observation$pharmaverse)) else "",
    observation$components)
}
