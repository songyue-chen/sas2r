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

render_style_guidance <- function(config = list()) {
  dialect <- config$dialect %||% "tidyverse"
  if (identical(dialect, "base")) {
    return(paste(c(
      "Style: base R first, faithful always.",
      paste("Prefer base R and the bundle helpers. Use an allowlisted package only where base R",
        "cannot express the SAS behavior faithfully or clearly; that choice is correct, not a defect.")),
      collapse = "\n"))
  }
  if (!identical(dialect, "tidyverse")) return(paste("Style:", dialect))
  facts <- agent_package_facts(config$allowlist)
  installed <- names(facts$versions)[facts$versions != "unknown"]
  candidates <- intersect(names(STYLE_PACKAGE_ROLES), facts$allowed)
  preferred <- intersect(candidates, installed)
  absent <- setdiff(candidates, installed)
  paste(c(
    "Style: tidyverse first, faithful always.",
    if (length(preferred)) c(
      "Prefer these allowlisted, installed packages whenever their functions express the SAS behavior faithfully:",
      paste0("- ", preferred, "::* for ", STYLE_PACKAGE_ROLES[preferred])),
    if (length(absent)) paste0("Allowlisted but not installed here, so do not use: ",
      paste(absent, collapse = ", "), "."),
    paste("When no preferred package expresses the behavior faithfully, use base R or the bundle helpers;",
      "that choice is correct, not a defect. Never change SAS behavior to fit a package idiom."),
    paste("Bundle helpers stay mandatory where they carry SAS semantics: lib_read/lib_write for data access,",
      "sas_sort for order (SAS missing sorts first; dplyr::arrange puts NA last), sas_merge for MERGE/BY,",
      "chr_cmp and %notin% for comparisons that may involve missing values, sas_round and the format helpers for ROUND and PUT.")),
    collapse = "\n")
}
