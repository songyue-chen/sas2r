# These expectations come only from parsed SAS, never from an agent contract.
# MERGE cardinality reference: SAS Language Reference, MERGE statement,
# https://support.sas.com/documentation/cdl/en/lrdict/64316/HTML/default/a000202970.htm
# Each BY group emits max(n_left, n_right), restricted by the source IN filter.
source_population_specs <- function(project, component_ids) {
  stats::setNames(lapply(component_ids, function(id) {
    stmts <- component_statements(project, id)
    if (is.null(stmts) || !all(c("unit_id", "unit_type") %in% names(stmts))) return(list())
    units <- split(stmts, factor(stmts$unit_id, levels = unique(stmts$unit_id)))
    rules <- lapply(units, function(unit) {
      kind <- unit$unit_type[[1L]]
      if (kind != "data_step") {
        lineage <- project$lineage
        outputs <- if (is.data.frame(lineage)) unique(lineage$dataset[
          lineage$unit_id %in% unit$unit_id & lineage$role %in% c("creates", "reads")]) else character()
        return(list(status = "unverified", reason = paste0("source_", kind), outputs = outputs))
      }
      ir <- parse_data_step(unit)
      rule <- list(status = "unverified", reason = "unsupported_source_step",
                   outputs = ir$outputs, inputs = ir$inputs, by = ir$by,
                   unit_id = unit$unit_id[[1L]], route = ir$route)
      if (nrow(ir$blockers) || length(ir$outputs) != 1L) return(rule)
      kinds <- vapply(ir$steps, `[[`, character(1), "kind")
      if (ir$route == "datastep" && length(ir$inputs) == 1L &&
          all(kinds %in% c("assign", "if_assign", "keep", "drop", "rename"))) {
        rule$status <- "supported"
        rule$reason <- "set_row_count"
      } else if (ir$route == "merge" && length(ir$inputs) == 2L &&
                 length(ir$by) && all(grepl("^[a-z_][a-z0-9_]*$", ir$by)) &&
                 length(kinds) <= 1L && all(kinds == "merge_filter")) {
        # Interpret only the finite IN= forms the deterministic path supports.
        flags <- unname(ir$in_flags)
        keep <- "full"
        if (length(kinds)) {
          cond <- tolower(gsub("\\s+", " ", trimws(ir$steps[[1]]$cond)))
          forms <- c(flags[1], flags[2], paste(flags[1], "and", flags[2]),
                     paste(flags[2], "and", flags[1]),
                     paste(flags[1], "and not", flags[2]), paste(flags[2], "and not", flags[1]))
          keep <- c("left", "right", "both", "both", "left_only", "right_only")[match(cond, forms)]
        }
        if (!is.na(keep)) {
          rule$status <- "supported"
          rule$reason <- "merge_by_population"
          rule$keep <- keep
        }
      }
      rule
    })
    # Repeated writes, in-place steps, and source libref rebinding need step
    # identity beyond a member name. Defer that class instead of guessing.
    outputs <- unlist(lapply(rules, `[[`, "outputs"), use.names = FALSE)
    repeated <- outputs[duplicated(outputs)]
    librefs <- tolower(stmts$text[stmts$first_token == "libname"])
    rebound <- length(librefs) > 0L
    lapply(rules, function(rule) {
      if (rebound || any(rule$outputs %in% c(repeated, rule$inputs))) {
        rule$status <- "unverified"
        rule$reason <- "source_rewrite_or_rebinding"
      }
      rule
    })
  }), component_ids)
}

# Self-contained observer passed into the same callr process that runs the
# generated program. Observe canonical IO, keep all dataset rows local, and
# serialize only counts/status. Programs may inline intermediates: report those
# checks as unverified when the corresponding source datasets are unavailable.
observe_source_population <- function(specs, env) {
  results <- lapply(specs, function(s) list(unit_id = s$unit_id, outputs = s$outputs,
    status = "unverified", reason = if (identical(s$status, "supported")) "output_not_observed" else s$reason))
  noop <- function() NULL
  if (!length(specs) || !exists("lib_write", env, inherits = FALSE) ||
      !exists("lib_read", env, inherits = FALSE)) {
    return(list(restore = noop, finish = function() results))
  }
  read <- get("lib_read", env, inherits = FALSE)
  write <- get("lib_write", env, inherits = FALSE)
  inputs <- list()
  produced <- unique(unlist(lapply(specs, `[[`, "outputs"), use.names = FALSE))
  read_member <- function(name) {
    parts <- strsplit(name, ".", fixed = TRUE)[[1L]]
    tryCatch(read(parts[1], parts[2]), error = function(e) NULL)
  }
  # Capture before execution, including a target that reuses an input object.
  for (name in setdiff(unique(unlist(lapply(specs, `[[`, "inputs"))), produced)) inputs[name] <- list(read_member(name))
  normalize_keys <- function(df, by) {
    df <- as.data.frame(df)
    names(df) <- tolower(names(df))
    if (!all(by %in% names(df))) return(NULL)
    df <- df[by]
    if (any(vapply(df, function(x) is.numeric(x) && any(haven::is_tagged_na(x)), logical(1)))) return(NULL)
    df[] <- lapply(df, function(x) {
      if (is.character(x)) { x <- sub(" +$", "", x); x[is.na(x)] <- ""; x }
      else if (is.numeric(x)) as.numeric(x) else x
    })
    df
  }
  check <- function(rule, df) {
    data <- lapply(rule$inputs, function(name) {
      value <- inputs[[name]]
      if (is.null(value) && !name %in% produced) read_member(name) else value
    })
    if (!all(vapply(data, is.data.frame, logical(1)))) return(list(status = "unverified", reason = "source_input_unavailable"))
    if (rule$route == "datastep") {
      expected <- nrow(data[[1]])
      return(list(status = if (nrow(df) == expected) "passed" else "failed",
                  reason = "set_row_count", expected_rows = expected, actual_rows = nrow(df)))
    }
    keys <- lapply(c(data, list(df)), normalize_keys, by = rule$by)
    if (any(vapply(keys, is.null, logical(1)))) return(list(status = "unverified", reason = "by_keys_unavailable_or_special_missing"))
    combined <- do.call(vctrs::vec_rbind, keys)
    groups <- vctrs::vec_group_id(combined)
    n <- attr(groups, "n")
    a_end <- nrow(keys[[1]]); b_end <- a_end + nrow(keys[[2]])
    a <- tabulate(groups[seq_len(a_end)], nbins = n)
    b <- tabulate(groups[a_end + seq_len(nrow(keys[[2]]))], nbins = n)
    actual <- tabulate(groups[b_end + seq_len(nrow(df))], nbins = n)
    keep <- switch(rule$keep, left = a > 0L, right = b > 0L,
      both = a > 0L & b > 0L, left_only = a > 0L & b == 0L,
      right_only = b > 0L & a == 0L, full = rep(TRUE, n))
    expected <- pmax(a, b) * keep
    list(status = if (identical(as.integer(actual), as.integer(expected))) "passed" else "failed",
         reason = "merge_by_population", expected_rows = sum(expected),
         actual_rows = nrow(df), mismatched_by_groups = sum(actual != expected))
  }
  observed_write <- function(df, libref, member, ...) {
    name <- tolower(paste(libref, member, sep = "."))
    for (i in seq_along(specs)) {
      rule <- specs[[i]]
      if (identical(rule$status, "supported") && name %in% rule$outputs) {
        result <- check(rule, df)
        results[[i]] <<- c(list(unit_id = rule$unit_id, outputs = rule$outputs), result)
        if (identical(result$status, "failed")) {
          stop(structure(list(message = sprintf(
            "Source population check failed for %s (%s): expected %d rows, got %d; mismatched BY groups: %s",
            name, result$reason, result$expected_rows, result$actual_rows,
            if (is.null(result$mismatched_by_groups)) "n/a" else result$mismatched_by_groups),
            call = NULL, population_check = results[[i]]),
            class = c("sas2r_population_mismatch", "error", "condition")))
        }
      }
    }
    value <- write(df, libref, member, ...)
    # Later source steps consume the newly written intermediate.
    inputs[name] <<- list(df)
    invisible(value)
  }
  assign("lib_write", observed_write, env)
  list(restore = function() assign("lib_write", write, env), finish = function() results)
}
