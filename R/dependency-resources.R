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

# A default takes effect at invocations that omit its keyword. If the
# definition or call cannot be located unambiguously, retain the source finding.
late_default_calls <- function(project, site, dataset) {
  defs <- project$macros$defs
  def <- defs[defs$unit_id == site$unit_id, ]
  if (nrow(def) != 1L || sum(defs$name == def$name) != 1L) return(NULL)
  params <- tryCatch(parse_macro_contract(def$name, def$params)$parameters,
    error = function(e) NULL)
  keywords <- tolower(params$name[tolower(params$sas_default) == dataset])
  if (!length(keywords)) return(NULL)
  code <- project$statements
  direct <- code$first_token == paste0("%", def$name)
  if (any(direct & code$unit_type == "macro_def")) return(NULL)
  calls <- code[direct, ]
  registered <- project$macros$calls
  registered <- registered[registered$name == def$name, ]
  if (any(!paste(registered$source_file, registered$line) %in% paste(calls$file, calls$line_start)))
    return(NULL)
  if (!nrow(calls)) return(NULL)
  overridden <- vapply(calls$text, function(text) {
    # Only skip a default for explicit top-level keyword arguments.
    text <- trimws(text)
    start <- regexpr("(", text, fixed = TRUE)
    if (start < 0L || !endsWith(text, ")")) return(FALSE)
    args <- substr(text, start + 1L, nchar(text) - 1L)
    scan <- sas_scan(args)
    ranges <- tryCatch(macro_parameter_ranges(args, scan$chars, scan$mask),
      error = function(e) NULL)
    if (is.null(ranges)) return(FALSE)
    supplied <- vapply(seq_len(nrow(ranges)), function(i) {
      first <- ranges[i, "start"]
      equals <- macro_parameter_equals(scan$chars, scan$mask, first, ranges[i, "end"])
      if (is.na(equals)) "" else tolower(trimws(substr(args, first, equals - 1L)))
    }, character(1))
    all(keywords %in% supplied)
  }, logical(1))
  calls[!overridden, ]
}

# Positions are shared across all sites in the declared execution sequence.
# Seed files are checked separately at the time of the dependency report.
late_writer_observations <- function(history, site, dataset, path, lineage, writers, advisory_ok) {
  visits <- history[[paste("site", site$unit_id, site$stmt_id)]]
  if (!length(visits)) return("project_writer_order_unresolved")
  positions <- history[paste("writer", writers)]
  writes <- unlist(positions, use.names = FALSE)
  rows <- rep(writers, lengths(positions))
  possible <- unlist(history[c(paste("possible", writers),
    paste("macro", paste(dataset, path, sep = "\r")))], use.names = FALSE)
  catalog <- c(history[[paste("catalog", path, sep = "\r")]],
    history[[paste("catalog", path, sub("^[^.]+[.]", "", dataset), sep = "\r")]])
  hard <- c(history[["unresolved"]], catalog)
  vapply(visits, function(at) {
    before <- which(writes < at)
    if (!length(before)) {
      if (any(possible < at) || any(catalog < at)) return("project_writer_order_unresolved")
      return("external_seed")
    }
    i <- before[which.max(writes[before])]
    if (lineage$dataset[rows[i]] != dataset) return("project_writer_binding_unresolved")
    since <- function(events) any(events > writes[i] & events < at)
    if (since(hard)) return("project_writer_order_unresolved")
    if (isTRUE(site$indirect) || since(history[["unknown"]]) || since(possible)) {
      if (advisory_ok) "project_writer_needs_execution" else "project_writer_order_unresolved"
    } else "available_from_project_writer"
  }, character(1))
}

# A late dataset report can refer to a literal passed to a macro even when the
# scanner cannot expand the macro into a read edge. Use the library at the
# caller's source location and the same member lookup as the runtime.
late_dataset_availability <- function(project, component_id, datasets) {
  datasets <- unique(tolower(datasets))
  datasets <- datasets[grepl("^[a-z_][a-z0-9_]*[.][a-z_][a-z0-9_]*$", datasets) &
    !startsWith(datasets, "work.")]
  registry <- project$libref_registry
  if (!length(datasets) || is.null(registry)) return(character())
  statements <- component_statements(project, component_id)
  if (is.null(statements)) return(character())
  rows <- which(statements$type == "code")
  tokens <- lapply(statements$text[rows], function(text) tolower(dataset_tokens(text)$lex))
  lineage <- project$lineage
  members <- sub("^[^.]+[.]", "", lineage$dataset)
  writer_rows <- which(lineage$role == "creates" & !startsWith(lineage$dataset, "work.") &
    members %in% sub("^[^.]+[.]", "", datasets))
  if (length(writer_rows)) {
    bindings <- effective_librefs(project)$bindings
    bound <- bindings[match(lineage$binding_id, bindings$binding_id), ]
    paths <- bound$selected_path
    paths[!bound$status %in% "bound" | bound$context_truncated %in% TRUE] <- NA_character_
    paths[startsWith(lineage$dataset, "work.")] <- "<session work>"
  }
  ordered <- project$config$migration$execution_order
  # Older or unordered project objects can still supply a single-root walk.
  # Build each required sequence at most once within this reconciliation.
  histories <- new.env(parent = emptyenv())
  writer_evidence <- function(site, dataset, path, same) {
    roots <- if (!is.null(ordered)) configured_execution_files(project) else execution_root_files(project)
    if (is.null(ordered) && length(roots) != 1L) {
      if (!site$file %in% roots || any(lineage$file[same] != site$file))
        return("project_writer_order_unresolved")
      roots <- site$file
    }
    history <- project$late_order
    if (is.null(history) || !identical(roots, project$ordered_roots)) {
      key <- paste(roots, collapse = "\r")
      if (!exists(key, histories, inherits = FALSE)) {
        histories[[key]] <- ordered_dataset_producers(project, paths,
          paste(lineage$dataset, paths, sep = "\r"), roots)$late_order
      }
      history <- histories[[key]]
    }
    late_writer_observations(history, site, dataset, path, lineage, same, !is.null(ordered))
  }
  vapply(stats::setNames(datasets, datasets), function(dataset) {
    sites <- rows[vapply(tokens, function(token) dataset %in% token, logical(1))]
    if (!length(sites)) return("no_literal_source_reference")
    # Scanned reads and destinations already have dependency checks of their own.
    ordinary <- vapply(sites, function(i) {
      refs <- dataset_statement_refs(statements$text[i], statements$first_token[i],
                                     statements$unit_type[i])
      dataset %in% tolower(c(refs$reads, refs$creates))
    }, logical(1))
    sites <- sites[!ordinary]
    if (!length(sites)) return("scanned_dataset_reference")
    parts <- split_ds(dataset)
    writes <- writer_rows[members[writer_rows] == parts[["member"]]]
    check_site <- function(site) {
      binding <- libref_binding_at(registry, parts[["lib"]], site$file, site$line_start)
      if (!identical(binding$status, "bound") || isTRUE(binding$context_truncated))
        return("library_binding_unresolved")
      source <- "external_seed"
      if (length(writes)) {
        if (any(is.na(paths[writes]) & lineage$dataset[writes] == dataset))
          return("project_writer_binding_unresolved")
        same <- writes[which(paths[writes] == binding$selected_path)]
        if (length(same)) source <- writer_evidence(site, dataset, binding$selected_path, same)
      }
      if ("external_seed" %in% source) {
        target <- tryCatch(sas2r_lib_member_file(list(read_path = binding$selected_path),
          parts[["member"]]), error = function(e) NULL)
        source[source == "external_seed"] <- if (!is.null(target) && file.access(target$path, 4L) == 0L)
          "available_in_bound_library" else "member_unavailable_in_bound_library"
      }
      source
    }
    status <- unlist(lapply(sites, function(i) {
      at <- statements[i, ]
      is_default <- at$first_token == "%macro"
      if (is_default) {
        calls <- late_default_calls(project, at, dataset)
        if (!is.null(calls)) at <- calls
      }
      if (!nrow(at)) return("scanned_dataset_reference")
      unlist(lapply(seq_len(nrow(at)), function(j) {
        site <- at[j, ]
        site$indirect <- is_default || site$stmt_id !=
          project$statements$stmt_id[match(site$unit_id, project$statements$unit_id)]
        check_site(site)
      }), use.names = FALSE)
    }), use.names = FALSE)
    # One unusable invocation blocks the component even if another can run.
    priority <- c("library_binding_unresolved", "project_writer_binding_unresolved",
      "project_writer_order_unresolved", "member_unavailable_in_bound_library",
      "project_writer_needs_execution", "available_from_project_writer",
      "available_in_bound_library", "scanned_dataset_reference")
    intersect(priority, status)[1L]
  }, character(1))
}

source_macro_variable_names <- function(text) {
  unique(tolower(sub("^&", "", unlist(regmatches(text,
    gregexpr("&[A-Za-z_][A-Za-z0-9_]*", text, perl = TRUE))))))
}

# Macro variables assigned by any scanned code statement (%LET, %GLOBAL,
# %LOCAL, CALL SYMPUT/SYMPUTX, INTO :name). A reported symbol with a project
# definition has a producer in the source; it is not a missing dependency.
# Names built from other macro variables (%global &sym) stay unresolved.
project_macro_variable_definitions <- function(project) {
  stmts <- project$statements
  if (is.null(stmts) || !is.data.frame(stmts) || !nrow(stmts)) return(character())
  text <- tolower(stmts$text[stmts$type == "code"])
  capture <- function(pattern, x = text) {
    unlist(lapply(regmatches(x, gregexec(pattern, x, perl = TRUE)), function(m)
      if (is.matrix(m) && ncol(m)) m[2L, ] else character()), use.names = FALSE)
  }
  scoped <- capture("%(?:global|local)\\s+([a-z_][a-z0-9_ ]*)")
  names <- c(
    capture("%let\\s+([a-z_][a-z0-9_]*)\\s*="),
    unlist(strsplit(trimws(scoped), "\\s+")),
    capture("call\\s+symputx?\\s*\\(\\s*['\"]([a-z_][a-z0-9_]*)['\"]"),
    capture(":([a-z_][a-z0-9_]*)", text[grepl("\\binto\\s*:", text, perl = TRUE)]))
  unique(names[nzchar(names)])
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
