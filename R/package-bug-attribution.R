# Attribution facts for AI bug triage (migration.bug_reports). They are built
# after the run has finished and only when the option is on. Nothing here
# feeds repair, candidate selection or run status.
#
# The facts say which party handled each differing column and what sas2r itself
# decided for the output. They are structural: column, dataset, program, helper
# and check names, type words and fixed vocabulary. No number, no cell or key
# value and no path is included, and package_bug_fact_violations() holds every
# block to that before it can reach a model.

# The steps of a migration that sas2r itself owns. A finding names one.
PACKAGE_BUG_STEPS <- c("reading_inputs_and_references", "facts_given_to_translator",
  "rule_based_translation_and_support_files", "runtime_helpers",
  "write_and_read_conversions", "scheduling_and_execution", "comparison")

# A SAS or R name that can be sent: a column created from data values, for
# example by a transpose, does not fit and is left out.
PACKAGE_BUG_NAME <- "^[A-Za-z_][A-Za-z0-9_.]{0,63}$"

# Days from 1960-01-01, where SAS dates start, to 1970-01-01, where R's do.
SAS_EPOCH_DAYS <- 3653

# Calls whose named arguments create or replace the columns they name.
R_COLUMN_DEFINING_CALLS <- c("mutate", "transmute", "summarise", "summarize", "reframe",
  "rename", "select", "tibble", "data.frame", "add_column", "cbind", "transform")

# Calls that can change a column without writing its name, so that a column the
# code never names cannot be called passed through.
R_UNNAMED_COLUMN_CALLS <- c("across", "mutate_all", "mutate_if", "mutate_at",
  "summarise_all", "summarise_if", "summarise_at", "summarize_all", "summarize_if",
  "summarize_at", "rename_with", "rename_all", "rename_if", "rename_at", "setnames",
  "pivot_longer", "pivot_wider", "gather", "spread", "do.call", "eval", "assign",
  "type.convert", ":=")

#' What R code does with named columns, read from the parsed code
#'
#' The code is parsed and never run. A column is `assigned` when a visible
#' assignment names it, `named` when the code mentions it without assigning it,
#' and `not_named` otherwise. `unnamed` is `TRUE` when the code also changes
#' columns without naming them.
#'
#' @param code R code as one string.
#' @param columns Column names; matching ignores case.
#' @return `list(columns, parsed, unnamed, helpers)`. Each column has `role`,
#'   the runtime `helpers` whose calls name it or produce its value, and nothing
#'   else. `helpers` at the top lists every runtime helper the code calls.
#' @noRd
r_column_use <- function(code, columns) {
  columns <- unique(tolower(columns))
  record <- function(role) list(role = role, helpers = character())
  use <- stats::setNames(lapply(columns, function(x) record("not_named")), columns)
  code <- paste(code %||% "", collapse = "\n")
  exprs <- if (nzchar(trimws(code))) tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) {
    return(list(columns = lapply(use, function(x) record("unknown")), parsed = FALSE,
      unnamed = FALSE, helpers = character()))
  }
  nested <- function(x) is.call(x) || is.expression(x) || is.pairlist(x)
  # The parts of a call, without the empty argument of one such as x[, "a"].
  children <- function(x) {
    x <- as.list(x)
    x[!vapply(seq_along(x), function(i) identical(x[[i]], quote(expr = )), logical(1))]
  }
  head_name <- function(e) {
    head <- e[[1L]]
    if (is.name(head)) return(as.character(head))
    if (is.call(head) && length(head) == 3L && is.name(head[[1L]]) &&
        as.character(head[[1L]]) %in% c("::", ":::") && is.name(head[[3L]])) return(as.character(head[[3L]]))
    ""
  }
  # Symbols and the words of string constants, such as by = "id visit".
  words <- function(e) {
    found <- character()
    visit <- function(x) {
      if (is.name(x)) found <<- c(found, as.character(x))
      else if (is.character(x)) found <<- c(found, unlist(strsplit(x[!is.na(x) & nchar(x) <= 200L], "[^A-Za-z0-9_.]+")))
      else if (nested(x)) for (child in children(x)) visit(child)
    }
    visit(e)
    unique(tolower(found[nzchar(found)]))
  }
  helpers_in <- function(e) {
    found <- character()
    visit <- function(x) {
      if (is.call(x) && head_name(x) %in% SAS2R_HELPER_NAMES) found <<- c(found, head_name(x))
      if (nested(x)) for (child in children(x)) visit(child)
    }
    visit(e)
    unique(found)
  }
  # The columns a replacement target names. NA marks a computed position.
  target_columns <- function(lhs) {
    if (is.name(lhs)) return(tolower(as.character(lhs)))
    if (!is.call(lhs) || length(lhs) < 2L) return(character())
    op <- head_name(lhs)
    if (op %in% c("$", "@") && length(lhs) == 3L) return(tolower(as.character(lhs[[3L]])))
    if (op %in% c("[[", "[")) {
      index <- children(lhs)[-(1:2)]
      literal <- unlist(lapply(index, function(i) {
        if (is.character(i)) return(i)
        if (is.call(i) && identical(head_name(i), "c") &&
            all(vapply(children(i)[-1L], is.character, logical(1)))) return(unlist(children(i)[-1L]))
        NULL
      }))
      return(if (length(literal)) tolower(literal) else NA_character_)
    }
    # attr(x$col, "label") <- ..., levels(x$col) <- ... and the like.
    target_columns(lhs[[2L]])
  }
  unnamed <- FALSE
  called <- character()
  assign_column <- function(column, value) {
    if (!column %in% columns) return(invisible())
    use[[column]]$role <<- "assigned"
    use[[column]]$helpers <<- union(use[[column]]$helpers, helpers_in(value))
  }
  walk <- function(e) {
    if (is.call(e)) {
      op <- head_name(e)
      if (op %in% c("<-", "=", "<<-") && length(e) == 3L) {
        lhs <- e[[2L]]
        targets <- target_columns(lhs)
        if (anyNA(targets)) unnamed <<- TRUE
        if (is.call(lhs) && head_name(lhs) %in% c("names", "colnames", "dimnames")) unnamed <<- TRUE
        for (column in targets[!is.na(targets)]) assign_column(column, e[[3L]])
      } else if (op %in% R_COLUMN_DEFINING_CALLS) {
        arguments <- as.list(e)[-1L]
        labels <- names(arguments) %||% rep("", length(arguments))
        for (i in which(nzchar(labels))) assign_column(tolower(labels[[i]]), arguments[[i]])
      }
      if (tolower(op) %in% R_UNNAMED_COLUMN_CALLS) unnamed <<- TRUE
      if (op %in% SAS2R_HELPER_NAMES) {
        called <<- union(called, op)
        for (column in intersect(words(e), columns)) {
          use[[column]]$helpers <<- union(use[[column]]$helpers, op)
        }
      }
    }
    if (nested(e)) for (child in children(e)) walk(child)
  }
  scanned <- tryCatch({ walk(exprs); words(exprs) }, error = function(e) NULL)
  if (is.null(scanned)) {
    return(list(columns = lapply(use, function(x) record("unknown")), parsed = FALSE,
      unnamed = FALSE, helpers = character()))
  }
  for (column in columns) {
    if (identical(use[[column]]$role, "not_named") && column %in% scanned) use[[column]]$role <- "named"
  }
  list(columns = use, parsed = TRUE, unnamed = unnamed, helpers = called)
}

#' What SAS source does with named columns, read from its statements
#'
#' A text scan, not a SAS parser. `assigned` covers an assignment or sum
#' statement, a SQL alias, a RENAME or OUTPUT target and a LENGTH, RETAIN,
#' INPUT or ARRAY declaration. Source that uses macro text can build a name
#' that is never written out, so a column it does not name is `unknown`.
#'
#' @param text SAS source as one string.
#' @param columns Column names; matching ignores case.
#' @return A named character vector of `assigned`, `named_not_assigned`,
#'   `not_named` or `unknown`.
#' @noRd
sas_column_use <- function(text, columns) {
  columns <- unique(tolower(columns))
  text <- paste(text %||% "", collapse = "\n")
  if (!nzchar(trimws(text))) return(stats::setNames(rep("unknown", length(columns)), columns))
  code <- tolower(gsub("(?s)/\\*.*?\\*/", " ", text, perl = TRUE))
  macro_text <- grepl("[&%]", code)
  # A name joined to a macro variable, as in &prefix.flag, is another name.
  code <- gsub("&+[a-z_][a-z0-9_]*[.]?", "_", code, perl = TRUE)
  statements <- trimws(gsub("[[:space:]]+", " ", strsplit(code, ";", fixed = TRUE)[[1L]]))
  lead <- "^(?:else )?(?:if .*? then )?(?:else )?(?:do )?"
  vapply(stats::setNames(columns, columns), function(column) {
    word <- paste0("(?<![a-z0-9_])", gsub(".", "\\.", column, fixed = TRUE), "(?![a-z0-9_])")
    hits <- statements[grepl(word, statements, perl = TRUE)]
    if (!length(hits)) return(if (macro_text) "unknown" else "not_named")
    assigned <- grepl(paste0(lead, word, " ?(?:\\[[^]]*\\]|\\{[^}]*\\})? ?(?:=(?!=)|\\+)"), hits, perl = TRUE) |
      grepl(paste0("\\bas ", word), hits, perl = TRUE) |
      grepl(paste0("^(?:length|retain|input|array)\\b.*", word), hits, perl = TRUE) |
      (grepl("^(?:rename|output)\\b|\\brename ?= ?\\(", hits, perl = TRUE) &
        grepl(paste0("= ?", word), hits, perl = TRUE))
    if (any(assigned)) "assigned" else "named_not_assigned"
  }, "")
}

# How a selected revision came to be. The record owns the answer: a repair
# carries the revision it replaced, a first translation carries the status of
# the model request, which is NA when the rules translated the program alone.
revision_origin <- function(revision) {
  if (!is.list(revision)) return("unknown")
  if (!is.null(revision$prior_revision_id)) {
    return(if (isTRUE(revision$helper_changed)) "model_repair_with_helper_patch" else "model_repair")
  }
  status <- revision$agent_status
  if (!is.character(status) || length(status) != 1L) return("unknown")
  if (is.na(status)) return("rule_based")
  if (!identical(status, "ok")) return("rule_based_model_unavailable")
  if (is.null(revision$mechanical_retry)) "model" else "model_after_check_retry"
}

# Whether the runtime helpers the bundle ran with are the ones sas2r ships.
runtime_helper_origin <- function(state) {
  path <- state$runtime$helpers
  shipped <- system.file("templates", "sas2r-helpers.R", package = "sas2r")
  if (!is_scalar_character(path) || !file.exists(path) || !nzchar(shipped)) return("unknown")
  definitions <- function(file) helper_definitions(paste(readLines(file, warn = FALSE), collapse = "\n"))
  tryCatch(if (identical(definitions(path), definitions(shipped))) "package" else "patched_during_run",
    error = function(e) "unknown")
}

# Upstream components are programs, macros and setup files, but also the
# external inputs a target depends on and the output node itself. Only the
# first kind has code to scan or show.
package_bug_program_ids <- function(state, ids) {
  nodes <- state$graph$nodes
  is_program <- vapply(ids, function(cid) {
    kinds <- nodes$type[!is.na(nodes$component_id) & nodes$component_id == cid]
    cid %in% names(state$selected_revisions) ||
      (length(kinds) > 0L && !all(kinds %in% c("external_input", "final_output")))
  }, logical(1))
  ids[is_program]
}

package_bug_external_inputs <- function(state, ids) {
  nodes <- state$graph$nodes
  inputs <- unique(nodes$component_id[!is.na(nodes$component_id) & nodes$component_id %in% ids &
    nodes$type %in% "external_input"])
  utils::head(inputs[grepl(PACKAGE_BUG_NAME, inputs)], 4L)
}

# Provenance of each program whose code is in the request.
package_bug_program_facts <- function(state, ids) {
  manifest <- state$baseline$manifest
  lapply(ids, function(cid) {
    revision <- state$selected_revisions[[cid]]
    tiers <- if (is.data.frame(manifest) && is_scalar_character(revision$staged_file)) {
      manifest$tier[!is.na(manifest$staged_file) & manifest$staged_file == revision$staged_file]
    } else character()
    list(component = cid, origin = revision_origin(revision),
      rule_based_units = if (!length(tiers)) "unknown" else if (all(tiers == "t1")) "all" else
        if (any(tiers == "t1")) "some" else "none")
  })
}

# The comparison policy the gate applied to one target: the project's rules
# with the target's own requirements on top.
target_comparison_policy <- function(state, key) {
  rules <- state$comparison_rules %||% state$config$comparison_rules %||% list()
  contracts <- state$assessment$contracts %||% state$output_contracts
  row <- if (is.data.frame(contracts) && key %in% contracts$target_key) {
    contracts[match(key, contracts$target_key), , drop = FALSE]
  }
  assertions <- if (!is.null(row) && is.list(row$assertions) && length(row$assertions)) row$assertions[[1L]]
  list(policy = effective_dataset_policy(rules, assertions %||% list()),
    logical_name = if (!is.null(row)) row$logical_name[[1L]] else NA_character_)
}

read_assessed_dataset <- function(path) {
  if (!is_scalar_character(path) || !file.exists(path)) return(NULL)
  data <- tryCatch(switch(tolower(tools::file_ext(path)),
    rds = readRDS(path), xpt = haven::read_xpt(path), sas7bdat = haven::read_sas(path), NULL),
    error = function(e) NULL)
  if (inherits(data, "data.frame")) data
}

# The number SAS itself stores for a value of this R class, with the name of
# the rule. NULL when SAS defines no such number for the class.
sas_stored_number <- function(x) {
  if (inherits(x, "Date")) return(list(values = as.numeric(unclass(x)) + SAS_EPOCH_DAYS,
    conversion = "sas_date_day_number"))
  if (inherits(x, "POSIXct")) return(list(values = as.numeric(unclass(x)) + SAS_EPOCH_DAYS * 86400,
    conversion = "sas_datetime_seconds"))
  if (inherits(x, "difftime") || inherits(x, "hms")) return(list(values = as.numeric(x, units = "secs"),
    conversion = "sas_time_seconds"))
  if (is.logical(x)) return(list(values = as.numeric(x), conversion = "sas_boolean_number"))
  NULL
}

#' Whether a type difference holds the same values in another representation
#'
#' Reads the generated dataset and the reference on this machine. A column
#' whose types differ is never compared by the gate. Here the side SAS would
#' hold as a number (a date as a day count from 1960, a datetime or time as
#' seconds, a true/false as 1/0) is converted to that number and the two
#' datasets are compared again with the gate's own row pairing and tolerance.
#' A failed `types` requirement between integer and double is answered from
#' the generated values alone. Only yes, no or not checked leaves this function.
#'
#' @param state Migration state.
#' @param target One output assessment.
#' @param key Target key.
#' @param columns Differing-column records with `name`, `difference`,
#'   `reference_type`, `generated_type` and `required_type`.
#' @return A named list by column of `list(representation_only, conversion)`.
#' @noRd
sas_representation_check <- function(state, target, key, columns) {
  unchecked <- list(representation_only = "not_checked")
  result <- stats::setNames(lapply(columns, function(x) unchecked), vapply(columns, `[[`, "", "name"))
  typed <- Filter(function(x) identical(x$difference, "type"), columns)
  if (!length(typed)) return(result)
  generated <- read_assessed_dataset(target$candidate_path)
  if (is.null(generated)) return(result)
  names(generated) <- tolower(names(generated))
  if (anyDuplicated(names(generated))) return(result)

  # A storage requirement: the same whole numbers held as double or integer.
  for (column in typed) {
    required <- column$required_type
    values <- generated[[column$name]]
    if (is.null(required) || is.null(values) || !is.numeric(values) || is.object(values)) next
    stored <- typeof(values)
    if (identical(required, "double") && identical(stored, "integer")) {
      result[[column$name]] <- list(representation_only = "yes", conversion = "numeric_storage")
    } else if (identical(required, "integer") && identical(stored, "double")) {
      present <- values[!is.na(values)]
      whole <- all(is.finite(present) & present == trunc(present) & abs(present) <= .Machine$integer.max)
      result[[column$name]] <- list(representation_only = if (whole) "yes" else "no",
        conversion = "numeric_storage")
    }
  }

  compared <- Filter(function(x) !is.null(x$reference_type) && !is.null(x$generated_type), typed)
  reference <- if (length(compared)) read_assessed_dataset(target$reference_path)
  if (is.null(reference)) return(result)
  names(reference) <- tolower(names(reference))
  if (anyDuplicated(names(reference))) return(result)
  rules <- character()
  for (column in compared) {
    name <- column$name
    if (!name %in% names(reference) || !name %in% names(generated)) next
    sides <- lapply(list(reference = reference[[name]], generated = generated[[name]]), function(x)
      if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x)
    numbers <- lapply(sides, sas_stored_number)
    plain <- vapply(sides, function(x) is.numeric(x) && !is.object(x), logical(1))
    # Exactly one side is a plain number and SAS defines the other's number.
    if (sum(plain) != 1L || is.null(numbers[[which(!plain)]])) next
    converted <- numbers[[which(!plain)]]
    if (plain[["reference"]]) {
      reference[[name]] <- sides$reference
      generated[[name]] <- converted$values
    } else {
      generated[[name]] <- sides$generated
      reference[[name]] <- converted$values
    }
    rules[[name]] <- converted$conversion
  }
  if (!length(rules)) return(result)
  setup <- target_comparison_policy(state, key)
  again <- tryCatch(compare_datasets_aligned(reference, generated,
    profile = dataset_comparison_profile(setup$policy), keys = setup$policy$keys,
    context = package_bug_alignment_context(state, setup$logical_name)), error = function(e) NULL)
  if (is.null(again)) return(result)
  for (name in names(rules)) {
    row <- again$vars[again$vars$var == name, , drop = FALSE]
    if (nrow(row) != 1L || !isTRUE(row$n_compared > 0)) next
    result[[name]] <- list(representation_only = if (row$n_mismatch == 0) "yes" else "no",
      conversion = rules[[name]])
  }
  result
}

# The alignment context the gate builds for a target, so that a second
# comparison pairs rows the way the first one did.
package_bug_alignment_context <- function(state, logical_name) {
  project <- state$project
  if (is.null(project)) return(NULL)
  build_output_alignment_context(list(logical_dataset = logical_name,
    contributing_unit_ids = unique(project$statements$unit_id)), project)
}

# The differing columns of one target, most structural first, each with the
# checks that failed for it. Names that do not fit PACKAGE_BUG_NAME are dropped.
package_bug_differing_columns <- function(target, summary = NULL, limit = 8L) {
  records <- list()
  add <- function(name, check, difference, ...) {
    name <- tolower(as.character(name %||% ""))
    for (one in name[!is.na(name) & nzchar(name)]) {
      if (is.null(records[[one]])) {
        records[[one]] <<- c(list(name = one, checks = check, difference = difference), list(...))
      } else {
        records[[one]]$checks <<- union(records[[one]]$checks, check)
        extra <- list(...)
        for (field in setdiff(names(extra), names(records[[one]]))) records[[one]][[field]] <<- extra[[field]]
      }
    }
  }
  # A type word is a class or storage name. One that is not a plain name says
  # nothing a model needs and is reported as "other".
  type <- function(x) if (is_scalar_character(x)) { if (grepl(PACKAGE_BUG_NAME, x)) x else "other" }
  structure <- target$differences$structure
  digest <- target$differences$digest
  for (x in comparison_records(structure$kind_mismatch %||% digest$structure$kind_mismatch)) {
    add(x$var, "reference_comparison", "type", reference_type = type(x$base_kind),
      generated_type = type(x$comp_kind))
  }
  types <- target$checks$types
  if (isFALSE(types$passed)) for (name in as.character(unlist(types$failed_columns))) {
    add(name, "types", "type", required_type = type(types$expected[[name]]),
      generated_type = type(types$actual[[name]]))
  }
  add(unlist(structure$only_base %||% digest$structure$vars_only_base), "reference_comparison", "missing_in_generated")
  if (isFALSE(target$checks$required_columns$passed)) {
    add(unlist(target$checks$required_columns$missing_columns), "required_columns", "missing_in_generated")
  }
  add(unlist(structure$only_comp %||% digest$structure$vars_only_comp), "reference_comparison", "extra_in_generated")
  described <- stats::setNames(summary$variables %||% list(),
    vapply(summary$variables %||% list(), function(v) tolower(v$name), ""))
  for (x in comparison_records(digest$vars)) {
    if (!isTRUE(as.numeric(x$n_mismatch) > 0)) next
    seen <- described[[tolower(x$var)]]
    add(x$var, "reference_comparison", "values", reference_type = type(x$kind), generated_type = type(x$kind),
      extent = seen$differences, patterns = if (length(seen$patterns)) seen$patterns)
  }
  for (field in c("labels", "formats")) {
    check <- target$checks[[field]]
    if (isFALSE(check$passed)) add(as.character(unlist(check$failed_columns)), field, "attribute")
  }
  sendable <- records[vapply(records, function(x) grepl(PACKAGE_BUG_NAME, x$name), logical(1))]
  kept <- utils::head(sendable, limit)
  others <- utils::head(setdiff(names(sendable), names(kept)), 12L)
  list(columns = lapply(unname(kept), function(x) x[!vapply(x, is.null, logical(1))]),
    others = others, more = length(kept) + length(others) < length(records))
}

#' Attribution facts for one failed dataset target
#'
#' For each differing column: the failed checks and types, whether the SAS and
#' the selected R of the upstream programs assign it, the runtime helpers
#' whose calls name it or produce it, what sas2r told the translator about it,
#' and whether a type difference holds the same values.
#'
#' @param state Migration state.
#' @param target One output assessment.
#' @param key Target key.
#' @param ids Upstream component ids of the target.
#' @param summary The reviewer discrepancy summary, or `NULL` when withheld.
#' @param limit Largest number of columns to describe.
#' @return `list(columns, other_differing_columns, more_columns, helpers_called,
#'   external_inputs)`; empty fields are left out.
#' @noRd
package_bug_attribution <- function(state, target, key, ids, summary = NULL, limit = 8L) {
  differing <- package_bug_differing_columns(target, summary, limit)
  columns <- differing$columns
  if (!length(columns)) return(list())
  column_names <- vapply(columns, `[[`, "", "name")
  ids <- unique(c(ids, unlist(lapply(ids, function(cid)
    context_component_dependencies(state$graph, cid)), use.names = FALSE)))
  inputs <- package_bug_external_inputs(state, ids)
  ids <- utils::head(package_bug_program_ids(state, ids), 8L)
  scans <- lapply(stats::setNames(ids, ids), function(cid) list(
    r = r_column_use(revision_code(state$selected_revisions[[cid]]), column_names),
    sas = sas_column_use(component_source_text(state$graph, cid), column_names)))
  parsed <- length(scans) > 0L && all(vapply(scans, function(s) s$r$parsed, logical(1)))
  unnamed <- any(vapply(scans, function(s) isTRUE(s$r$unnamed), logical(1)))
  told <- package_bug_translator_facts(state, ids, column_names)
  representation <- tryCatch(sas_representation_check(state, target, key, columns),
    error = function(e) list())
  columns <- lapply(columns, function(column) {
    name <- column$name
    r_roles <- vapply(scans, function(s) s$r$columns[[name]]$role, "")
    sas_roles <- vapply(scans, function(s) s$sas[[name]], "")
    in_output <- !identical(column$difference, "missing_in_generated")
    column$sas_source <- if (any(sas_roles == "assigned")) "assigned" else
      if (!length(scans) || any(sas_roles == "unknown")) "unknown" else
      if (any(sas_roles == "named_not_assigned")) "named_not_assigned" else "not_named"
    column$generated_code <- if (any(r_roles == "assigned")) "assigned" else
      if (!parsed || (in_output && unnamed)) "unknown" else
      if (in_output) "passed_through" else
      if (any(r_roles == "named")) "named_not_assigned" else "not_named"
    assigned_in <- utils::head(names(scans)[r_roles == "assigned"], 3L)
    if (length(assigned_in)) column$assigned_in <- assigned_in
    helpers <- utils::head(unique(unlist(lapply(scans, function(s) s$r$columns[[name]]$helpers),
      use.names = FALSE)), 4L)
    if (length(helpers)) column$helpers <- helpers
    if (length(told[[name]])) column$translator_told <- told[[name]]
    if (identical(column$difference, "type")) {
      checked <- representation[[name]] %||% list(representation_only = "not_checked")
      column$representation_only <- checked$representation_only
      column$conversion <- checked$conversion
    }
    column
  })
  called <- utils::head(unique(unlist(lapply(scans, function(s) s$r$helpers), use.names = FALSE)), 8L)
  facts <- list(columns = columns, other_differing_columns = differing$others,
    more_columns = if (isTRUE(differing$more)) TRUE, helpers_called = called, external_inputs = inputs)
  facts[lengths(facts) > 0L]
}

# What sas2r told the translator about a column: the type it inferred from the
# SAS source for the datasets the upstream programs read. The translator
# receives the same line as "input schemas (inferred from code references)".
package_bug_translator_facts <- function(state, ids, column_names) {
  project <- state$project
  lineage <- project$lineage
  if (is.null(project) || is.null(lineage) || !length(ids)) return(list())
  tryCatch({
    schemas <- infer_schemas(project)
    units <- unique(unlist(lapply(ids, function(cid) component_statements(project, cid)$unit_id)))
    reads <- unique(lineage$dataset[lineage$role == "reads" & lineage$unit_id %in% units])
    reads <- reads[grepl(PACKAGE_BUG_NAME, reads)]
    told <- lapply(stats::setNames(column_names, column_names), function(name) {
      hits <- list()
      for (dataset in reads) {
        vars <- schemas[[dataset]]$vars
        kind <- vars$kind[match(name, tolower(vars$var))]
        if (length(kind) == 1L && kind %in% c("numeric", "character")) {
          hits[[length(hits) + 1L]] <- list(input = dataset, type = kind)
        }
      }
      utils::head(hits, 2L)
    })
    told[lengths(told) > 0L]
  }, error = function(e) list())
}

PACKAGE_BUG_KEY_ORIGINS <- c("configured", "by", "sort", "merge", "lineage", "by_subset", "sort_subset",
  "known_identifiers", "heuristic")
PACKAGE_BUG_PAIRING_METHODS <- c("unique_key", "duplicate_key_multiset", "keyless_multiset")

# Where the comparison's row keys came from. The gate keeps the keys it used
# and the keys that were configured, but not the origin of a key it inferred.
# Key candidates depend only on the shared column names and the source
# context, so that origin is recovered without reading data. When configured
# keys repeat, the gate adds the first candidate that makes them unique on
# both sides; `refined_by` is that candidate's origin. A candidate's validity
# depends only on its key set, so the first candidate in order that gives the
# keys used is the one the gate chose.
package_bug_key_origin <- function(state, structure, policy, logical_name) {
  used <- tolower(as.character(unlist(structure$selected_keys)))
  if (!length(used)) return(list(from = "none"))
  configured <- tolower(as.character(unlist(structure$configured_keys %||% policy$keys)))
  if (length(configured) && identical(used, configured)) return(list(from = "configured"))
  common <- as.character(unlist(structure$common))
  frame <- as.data.frame(stats::setNames(rep(list(logical()), length(common)), common),
    check.names = FALSE, stringsAsFactors = FALSE)
  context <- tryCatch(package_bug_alignment_context(state, logical_name), error = function(e) NULL) %||%
    empty_alignment_context()
  candidates <- tryCatch(infer_alignment_keys(frame, frame, context = context), error = function(e) list())
  origin <- function(keys_of) {
    hit <- Filter(function(x) identical(keys_of(x), used), candidates)
    found <- if (length(hit)) hit[[1L]]$provenance
    if (isTRUE(found %in% setdiff(PACKAGE_BUG_KEY_ORIGINS, "configured"))) found else "unknown"
  }
  if (length(configured) && all(configured %in% used)) {
    return(list(from = "configured", refined_by = origin(function(x) union(configured, x$keys))))
  }
  list(from = origin(function(x) x$keys))
}

#' The decisions sas2r made for one failed dataset target
#'
#' How rows were paired and which keys were used, the numeric tolerance, the
#' file types of the reference and the generated dataset, and the date-column
#' decisions when the assessment records them.
#'
#' @param state Migration state.
#' @param target One output assessment.
#' @param key Target key.
#' @param column_names Names of the differing columns.
#' @return A list of decision records.
#' @noRd
package_bug_decisions <- function(state, target, key, column_names = character()) {
  setup <- tryCatch(target_comparison_policy(state, key), error = function(e) list(policy = list()))
  policy <- setup$policy
  structure <- target$differences$structure
  sendable <- function(x) {
    x <- tolower(as.character(unlist(x)))
    utils::head(x[grepl(PACKAGE_BUG_NAME, x)], 8L)
  }
  file_type <- function(path) {
    if (!is_scalar_character(path) || is.na(path)) return("none")
    type <- tolower(tools::file_ext(path))
    if (type %in% c("rds", "xpt", "sas7bdat")) type else "other"
  }
  decisions <- list()
  compared <- !is.null(target$checks$reference_comparison$summary)
  if (compared) {
    configured <- sendable(structure$configured_keys %||% policy$keys)
    used <- sendable(structure$selected_keys)
    order <- structure$order
    origin <- package_bug_key_origin(state, structure, policy, setup$logical_name)
    decisions$row_pairing <- c(list(
      method = if (isTRUE(structure$alignment_method %in% PACKAGE_BUG_PAIRING_METHODS))
        structure$alignment_method else "unknown",
      keys_used = used, keys_from = origin$from),
      if (!is.null(origin$refined_by)) list(keys_refined_by = origin$refined_by),
      list(keys_configured = configured,
        row_order = if (!isTRUE(order$meaningful)) "not_required" else
          if (isTRUE(order$order_equivalent)) "required_and_same" else "required_and_different"))
    profile <- tryCatch(dataset_comparison_profile(policy), error = function(e) NULL)
    if (!is.null(profile)) {
      set <- !is.null(policy$tol_abs) || !is.null(policy$tol_rel) || !is.null(policy$numeric_tolerance)
      decisions$numeric_tolerance <- list(source = if (set) "configured" else "package_default",
        absolute = if (isTRUE(profile$numeric$abs > 0)) "above_zero" else "zero",
        relative = if (isTRUE(profile$numeric$rel > 0)) "above_zero" else "zero",
        column_overrides = intersect(column_names, tolower(names(profile$overrides))))
    }
  }
  decisions$reference_file <- file_type(target$reference_path)
  decisions$generated_file <- file_type(target$candidate_path)
  dates <- package_bug_date_decisions(target$date_columns, column_names)
  if (length(dates)) decisions$date_columns <- dates
  decisions
}

# The date-column record of an assessment, for the differing columns only: a
# SAS date stored as an R Date or left as a day number, and why.
package_bug_date_decisions <- function(record, column_names) {
  rows <- c(lapply(record$typed, function(x) c(x, decision = "stored_as_date")),
    lapply(record$deferred, function(x) c(x, decision = "left_as_day_number")))
  text <- function(x, pattern) if (is_scalar_character(x) && nchar(x) <= 240L && grepl(pattern, x)) x
  records <- lapply(rows, function(x) {
    name <- tolower(as.character(x$var %||% ""))
    if (length(name) != 1L || !name %in% column_names) return(NULL)
    one <- list(name = name, decision = x$decision,
      basis = if (isTRUE(x$evidence %in% c("reference", "source"))) x$evidence else "unknown",
      # The gate records a format name without its period, such as DATE9.
      format = text(x$format, "^[$]?[A-Za-z_][A-Za-z0-9_]{0,31}([.][0-9]*)?$"),
      reason = text(x$reason, "^[^\r\n/\\\\]*$"))
    one[!vapply(one, is.null, logical(1))]
  })
  utils::head(Filter(Negate(is.null), records), 8L)
}

#' Difference classes that repeat across failed dataset targets
#'
#' The same type difference, value pattern or failed check in several outputs
#' has a shared cause. `programs` says whether those outputs also share an
#' upstream program, which would explain it without the package, and
#' `common_inputs` names the external inputs all of them read.
#'
#' @param state Migration state.
#' @param evidence_ids Named character vector, target key to evidence id.
#' @param limit Largest number of classes to report.
#' @return A list of class records, largest first; empty when nothing repeats.
#' @noRd
package_bug_shared_patterns <- function(state, evidence_ids = character(), limit = 5L) {
  targets <- Filter(function(t) identical(t$kind %||% "dataset", "dataset") && identical(t$status, "failed"),
    state$assessment$targets %||% list())
  targets <- targets[grepl(PACKAGE_BUG_NAME, names(targets))]
  if (length(targets) < 2L) return(list())
  word <- function(x) is_scalar_character(x) && grepl(PACKAGE_BUG_NAME, x)
  classes <- list()
  add <- function(id, class, key, column = NULL) {
    entry <- classes[[id]] %||% c(class, list(targets = character(), columns = character()))
    entry$targets <- union(entry$targets, key)
    if (word(column)) entry$columns <- union(entry$columns, tolower(column))
    classes[[id]] <<- entry
  }
  for (key in names(targets)) {
    target <- targets[[key]]
    digest <- target$differences$digest
    for (x in comparison_records(target$differences$structure$kind_mismatch %||% digest$structure$kind_mismatch)) {
      if (word(x$base_kind) && word(x$comp_kind)) add(paste("type", x$base_kind, x$comp_kind),
        list(class = "type_difference", reference_type = x$base_kind, generated_type = x$comp_kind), key, x$var)
    }
    differing <- vapply(Filter(function(v) isTRUE(as.numeric(v$n_mismatch) > 0), comparison_records(digest$vars)),
      function(v) v$var, "")
    for (x in comparison_records(digest$pattern_hints)) {
      if (!x$var %in% differing || !x$hint %in% REVIEWER_SUMMARY_VOCABULARY$patterns) next
      add(paste("pattern", x$hint), list(class = "value_pattern", pattern = x$hint), key, x$var)
    }
    failed <- names(Filter(function(check) isFALSE(check$passed), target$checks))
    for (check in setdiff(failed[vapply(failed, word, logical(1))], "reference_comparison")) {
      add(paste("check", check), list(class = "failed_check", check = check), key,
        if (length(unlist(target$checks[[check]]$failed_columns)) == 1L) unlist(target$checks[[check]]$failed_columns))
    }
  }
  classes <- Filter(function(x) length(x$targets) > 1L, classes)
  if (!length(classes)) return(list())
  classes <- utils::head(classes[order(-vapply(classes, function(x) length(x$targets), 1L))], limit)
  upstream <- lapply(stats::setNames(names(targets), names(targets)), function(key)
    state$assessment$lineage_by_target[[key]]$upstream_components %||%
      tryCatch(graph_output_writers(state$graph, key), error = function(e) character()))
  lapply(unname(classes), function(x) {
    programs <- lapply(upstream[x$targets], function(ids) package_bug_program_ids(state, ids))
    common <- Reduce(intersect, programs)
    x$programs <- if (any(lengths(programs) == 0L)) "unknown" else if (length(common)) "shared" else "independent"
    if (length(common)) x$common_programs <- utils::head(common, 3L)
    inputs <- Reduce(intersect, lapply(upstream[x$targets], function(ids) package_bug_external_inputs(state, ids)))
    if (length(inputs)) x$common_inputs <- inputs
    x$share_of_failed_outputs <- if (length(x$targets) == length(targets)) "all" else
      if (2L * length(x$targets) > length(targets)) "most" else "some"
    x$evidence <- unname(evidence_ids[intersect(x$targets, names(evidence_ids))])
    x$more_targets <- length(x$targets) > 5L
    x$targets <- utils::head(x$targets, 5L)
    x$columns <- utils::head(x$columns, 5L)
    x
  })
}

# The closed format of every fact block built in this file. Each field has a
# kind: a name (PACKAGE_BUG_NAME), a label (one short line), a value from a
# fixed vocabulary, or a list of records. Vectors are held to their element
# kind.
PACKAGE_BUG_FACT_FORMAT <- local({
  name <- list(kind = "name")
  label <- list(kind = "label")
  one_of <- function(...) list(kind = "vocabulary", values = c(...))
  records <- function(...) list(kind = "records", fields = list(...))
  record <- function(...) list(kind = "record", fields = list(...))
  step <- c("assigned", "named_not_assigned", "not_named", "unknown")
  list(
    columns = records(name = name, checks = name, difference = one_of("type", "values",
        "missing_in_generated", "extra_in_generated", "attribute"),
      reference_type = name, generated_type = name, required_type = name,
      extent = one_of(REVIEWER_SUMMARY_VOCABULARY$differences),
      patterns = one_of(REVIEWER_SUMMARY_VOCABULARY$patterns),
      sas_source = one_of(step), generated_code = one_of(step, "passed_through"),
      assigned_in = label, helpers = label,
      translator_told = records(input = name, type = one_of("numeric", "character")),
      representation_only = one_of("yes", "no", "not_checked"),
      conversion = one_of("sas_date_day_number", "sas_datetime_seconds", "sas_time_seconds",
        "sas_boolean_number", "numeric_storage")),
    other_differing_columns = name,
    more_columns = list(kind = "flag"),
    helpers_called = label,
    external_inputs = name,
    decisions = record(
      row_pairing = record(method = one_of(PACKAGE_BUG_PAIRING_METHODS, "unknown"),
        keys_used = name, keys_configured = name,
        keys_from = one_of(PACKAGE_BUG_KEY_ORIGINS, "none", "unknown"),
        keys_refined_by = one_of(setdiff(PACKAGE_BUG_KEY_ORIGINS, "configured"), "unknown"),
        row_order = one_of("not_required", "required_and_same", "required_and_different")),
      numeric_tolerance = record(source = one_of("configured", "package_default"),
        absolute = one_of("zero", "above_zero"), relative = one_of("zero", "above_zero"),
        column_overrides = name),
      reference_file = one_of("rds", "xpt", "sas7bdat", "other", "none"),
      generated_file = one_of("rds", "xpt", "sas7bdat", "other", "none"),
      date_columns = records(name = name, decision = one_of("stored_as_date", "left_as_day_number"),
        basis = one_of("reference", "source", "unknown"), format = label, reason = label)),
    shared_patterns = records(class = one_of("type_difference", "value_pattern", "failed_check"),
      reference_type = name, generated_type = name,
      pattern = one_of(REVIEWER_SUMMARY_VOCABULARY$patterns), check = name,
      targets = name, columns = name, programs = one_of("shared", "independent", "unknown"),
      common_programs = label, common_inputs = name, share_of_failed_outputs = one_of("all", "most", "some"),
      evidence = label, more_targets = list(kind = "flag")),
    programs = records(component = label,
      origin = one_of("rule_based", "rule_based_model_unavailable", "model", "model_after_check_retry",
        "model_repair", "model_repair_with_helper_patch", "unknown"),
      rule_based_units = one_of("all", "some", "none", "unknown")),
    runtime_helpers = one_of("package", "patched_during_run", "unknown"))
})

#' Why a block of triage facts does not fit its closed format
#'
#' The reasons name fields only. They never quote a value, which could be the
#' very thing that should not leave the machine.
#'
#' @param facts A named list of blocks from `PACKAGE_BUG_FACT_FORMAT`.
#' @param paths Local paths that must not appear in any value.
#' @return Reasons as a character vector; empty when the facts fit.
#' @noRd
package_bug_fact_violations <- function(facts, paths = character()) {
  if (!is.list(facts) || (length(facts) && is.null(names(facts)))) return("the facts are not a named list")
  problems <- character()
  note <- function(...) problems <<- c(problems, paste0(...))
  strings <- function(x) is.character(x) && !anyNA(x) && all(nchar(x) <= 240L) && !any(grepl("[\r\n]", x))
  check <- function(value, format, where) {
    if (format$kind %in% c("record", "records")) {
      rows <- if (identical(format$kind, "record")) list(value) else value
      if (!is.list(value) || (identical(format$kind, "records") && !is.null(names(value)))) {
        return(note(where, " is not a list of records"))
      }
      for (row in rows) {
        if (!is.list(row) || (length(row) && is.null(names(row)))) { note(where, " holds something other than a record"); next }
        extra <- setdiff(names(row), names(format$fields))
        safe <- extra[grepl("^[A-Za-z_][A-Za-z0-9_.]{0,39}$", extra)]
        if (length(extra)) note("unexpected field in ", where, if (length(safe)) paste0(" (", paste(safe, collapse = ", "), ")"))
        for (field in intersect(names(row), names(format$fields))) {
          check(row[[field]], format$fields[[field]], paste0(where, "$", field))
        }
      }
      return(invisible())
    }
    ok <- switch(format$kind,
      name = strings(value) && all(grepl(PACKAGE_BUG_NAME, value)),
      label = strings(value),
      vocabulary = is.character(value) && all(value %in% format$values),
      flag = is.logical(value) && length(value) == 1L && !is.na(value),
      FALSE)
    if (!ok) note(where, " is outside its fixed format")
  }
  extra <- setdiff(names(facts), names(PACKAGE_BUG_FACT_FORMAT))
  if (length(extra)) note("unexpected block in the facts")
  for (block in intersect(names(facts), names(PACKAGE_BUG_FACT_FORMAT))) {
    check(facts[[block]], PACKAGE_BUG_FACT_FORMAT[[block]], block)
  }
  leaves <- unlist(facts, use.names = FALSE)
  if (any(unlist(rapply(facts, is.numeric, how = "list")))) note("a numeric value is present")
  paths <- paths[is.character(paths) & !is.na(paths) & nzchar(paths)]
  if (is.character(leaves) && any(vapply(paths, function(path) any(grepl(path, leaves, fixed = TRUE)), logical(1)))) {
    note("a local path is present")
  }
  unique(problems)
}

# Facts that fail the closed format never reach a model: the run goes on
# without them and the omission is recorded. Tests run with the strict option
# set, so a defect in a constructor fails there instead of dropping facts.
withhold_package_bug_facts <- function(key, problems, omitted = NULL) {
  key <- if (is.character(key) && length(key) == 1L && !is.na(key)) key else "unknown"
  if (isTRUE(getOption("sas2r.strict_discrepancy_summary", FALSE))) {
    cli::cli_abort(c("The bug triage facts for {.val {key}} are outside their closed format.",
      "x" = "{problems}"), class = "sas2r_package_bug_fact_violation")
  }
  if (is.environment(omitted)) {
    omitted$items <- c(omitted$items, list(list(type = "package_bug_facts_omitted",
      target = key, reasons = problems)))
  }
  cli::cli_warn(c("The bug triage facts for {.val {key}} were withheld from the model request.",
    "i" = "They did not fit the closed format a model is allowed to see: {problems}.",
    "i" = "The run continues without them. Please report this as a sas2r defect."),
    class = "sas2r_package_bug_facts_omitted")
  NULL
}

# Check a block and return it, or withhold it.
checked_package_bug_facts <- function(facts, key, paths = character(), omitted = NULL) {
  problems <- package_bug_fact_violations(facts, paths)
  if (!length(problems)) return(facts)
  withhold_package_bug_facts(key, problems, omitted)
}
