# SAS session metadata is an environment query, not a study-data producer.
# Keep the documented names explicit: SASHELP also contains ordinary sample
# datasets, so neither SASHELP.* nor SASHELP.V* is a safe blanket exemption.
# https://support.sas.com/documentation/cdl/en/sqlproc/63043/HTML/default/n02s19q65mw08gn140bwfdh7spx7.htm
SAS_METADATA_RESOURCES <- c(
  paste0("dictionary.", c(
    "catalogs", "check_constraints", "columns", "constraint_column_usage",
    "constraint_table_usage", "dataitems", "destinations", "dictionaries",
    "engines", "extfiles", "filters", "formats", "functions", "goptions",
    "indexes", "infomaps", "libnames", "macros", "members", "options",
    "referential_constraints", "remember", "styles", "table_constraints",
    "tables", "titles", "views", "view_sources")),
  paste0("sashelp.", c(
    "vcatalg", "vchkcon", "vcolumn", "vcncolu", "vcntabu", "vdatait",
    "vdest", "vdctnry", "vengine", "vextfl", "vfilter", "vformat",
    "vcformat", "vfunc", "vgopt", "vallopt", "vindex", "vinfomp",
    "vlibnam", "vmacro", "vmember", "vsacces", "vscatlg", "vslib",
    "vstable", "vstabvw", "vsview", "voption", "vrefcon", "vrememb",
    "vstyle", "vtabcon", "vtable", "vtitle", "vview"))
)

# Documented automatic variables, not a blanket SYS* exemption. Runtime-state
# variables are recognized too, but their producing operations still matter.
# https://support.sas.com/documentation/cdl/en/mcrolref/62978/HTML/default/p14ym6slnzfstzn1t9yp5v31ijis.htm
SAS_AUTOMATIC_VARIABLES <- c("sysdate", "sysdate9", "sysday", "systime",
  "sysscp", "sysscpl", "sysver", "sysvlong", "sysvlong4", "sysencoding",
  "syshostname", "sysjobid", "sysuserid", "sysncpu", "syslast", "sysdsn",
  "syserr", "syserrortext", "syswarningtext", "syscc", "sysnobs")
SAS_SEARCH_OPTIONS <- c("sasautos", "fmtsearch")

component_environment_resources <- function(project, component_id) {
  stmts <- component_statements(project, component_id)
  text <- tolower(paste(stmts$text[stmts$type == "code"], collapse = "\n"))
  names <- unlist(regmatches(text, gregexpr(
    "\\b(?:sashelp|dictionary)[.][a-z_][a-z0-9_]*\\b", text, perl = TRUE)))
  variables <- intersect(source_macro_variable_names(text), SAS_AUTOMATIC_VARIABLES)
  options <- SAS_SEARCH_OPTIONS[vapply(SAS_SEARCH_OPTIONS, function(name) {
    grepl(paste0("\\bgetoption\\s*\\(\\s*['\"]?", name, "\\b"), text, perl = TRUE) ||
      any(grepl(paste0("\\b", name, "\\s*="),
        tolower(stmts$text[stmts$first_token == "options"]), perl = TRUE))
  }, logical(1))]
  list(metadata = intersect(names, SAS_METADATA_RESOURCES), variables = variables,
    options = options, paths = configured_path_dependency_symbols(project, component_id))
}

filter_dependency_resources <- function(reported, project, component_id) {
  env <- component_environment_resources(project, component_id)
  symbols <- c(env$variables, env$paths)
  resources <- c(SAS_METADATA_RESOURCES, symbols, paste0("&", symbols),
    paste0("&", symbols, "."), env$options)
  reported[!tolower(trimws(reported)) %in% resources &
    !tolower(sub("^%", "", trimws(reported))) %in% MACRO_BUILTINS]
}

source_macro_variable_names <- function(text) {
  unique(tolower(sub("^&", "", unlist(regmatches(text,
    gregexpr("&[A-Za-z_][A-Za-z0-9_]*", text, perl = TRUE))))))
}

# Reuse the binding decision that supplies generated R and agent context. A
# configured directory resolves the LIBNAME path, not arbitrary uses of that
# macro variable (for example INFILE, a filter, or a dynamic dataset name).
configured_path_dependency_symbols <- function(project, component_id) {
  stmts <- component_statements(project, component_id)
  if (is.null(stmts) || is.null(project$libref_registry)) return(character())
  stmts <- stmts[stmts$type == "code", , drop = FALSE]
  bindings <- effective_librefs(project)$bindings
  bindings <- bindings[which(bindings$kind == "statement" &
    bindings$status == "bound" & bindings$selection_origin == "configured_fallback" &
    bindings$fallback_reason == "source_path_unresolved_macro"), , drop = FALSE]
  remaining <- stmts$text
  covered <- character()
  for (i in seq_len(nrow(bindings))) {
    b <- bindings[i, , drop = FALSE]
    rows <- which(stmts$file == b$use_file & stmts$line_start == b$use_line &
      stmts$first_token == "libname")
    for (row in rows) {
      declaration <- extract_librefs(stmts[row, , drop = FALSE])
      if (nrow(declaration) != 1L || declaration$libref != b$libref ||
          !identical(declaration$path_expression, b$source_path_expression)) next
      covered <- union(covered, source_macro_variable_names(b$source_path_expression))
      remaining[row] <- sub(b$source_path_expression, "", remaining[row], fixed = TRUE)
    }
  }
  # Single-quoted SAS strings do not expand macro variables. Comments were
  # already separated by the scanner; retain double-quoted and executable uses.
  remaining <- vapply(remaining, mask_strings, "", keep_double = TRUE)
  setdiff(covered, source_macro_variable_names(remaining))
}

render_dependency_resources <- function(project, component_id) {
  env <- component_environment_resources(project, component_id)
  metadata <- env$metadata
  paths <- env$paths
  c(
    if (length(paths)) c(
      paste0("LIBNAME path variables covered by the selected configured bindings: ",
        paste(paths, collapse = ", "), "."),
      "These path-only references do not require another program or a global R variable. Use the established library bindings; this does not resolve other uses of a macro variable."),
    if (length(metadata)) c(
      paste0("SAS session metadata resources: ", paste(metadata, collapse = ", "), "."),
      "These describe the SAS environment, not missing study datasets or upstream programs. Translate the required query behavior using available R/project context; do not invent metadata rows or treat an unsupported query as implemented. Keep unresolved behavior in uncertainty for review and execution checks."),
    if (length(env$variables)) c(paste("SAS automatic macro variables:", paste(env$variables, collapse = ", ")),
      "Translate their documented meaning, not just their name. Session date/time is fixed at run start; R's version is not SAS's version. SYSLAST/SYSDSN and status/count variables depend on prior operations: preserve those relationships. Do not fabricate the original SAS environment; report unavailable values or unsupported behavior in uncertainty."),
    if (length(env$options)) c(paste("SAS search options:", paste(env$options, collapse = ", ")),
      "SASAUTOS describes macro search locations; use configured/discovered macro sources. FMTSEARCH describes format catalog lookup; preserve catalog precedence and required custom formats. Recognizing the option does not supply missing macro files or formats."),
    component_readiness_context(project, component_id)
  )
}
