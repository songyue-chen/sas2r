#' Parse gate for R code
#'
#' Evaluates whether the generated R code passes linting without errors.
#'
#' @param code Character string of R code.
#' @return A list with `pass` (logical) and `lint` (tibble of lint results).
#' @noRd
gate_parse <- function(code) {
  lint <- lint_r_code(code)
  list(pass = !any(lint$level == "error"), lint = lint)
}

#' Check a generated program revision for mechanical validity
#'
#' Evaluates parse validity, package lint, allowed helper names, declared
#' parameter/function interface, and bootstrap/include consistency.
#'
#' @param r_path Path to the generated R program file.
#' @param contract Behavioral contract list or object.
#' @param registry Optional path to `autoexec.R` or registry object.
#' @param helper_patch Optional candidate shared-helper patch; checked without execution.
#' @return A list with `pass` (logical), `errors` (character vector),
#'   `warnings` (character vector), and `lint` (lint tibble).
#' @noRd
check_program_revision <- function(r_path, contract = NULL, registry = NULL, helper_patch = NULL) {
  errors <- character()
  warnings <- character()

  if (!is.character(r_path) || length(r_path) != 1L || !file.exists(r_path)) {
    return(list(
      pass = FALSE,
      errors = paste0("program file not found: ", as.character(r_path)),
      warnings = character(),
      lint = tibble::tibble(level = character(), kind = character(), detail = character())
    ))
  }

  code_lines <- readLines(r_path, warn = FALSE)
  code_text <- paste(code_lines, collapse = "\n")

  # 1. Parse check
  parsed <- tryCatch(parse(text = code_text), error = function(e) e)
  if (inherits(parsed, "error")) {
    errors <- c(errors, paste0("parse_error: ", conditionMessage(parsed)))
  }

  # 2. Package lint check (excluding the sas2r bootstrap block)
  code_for_lint <- code_text
  if (!inherits(parsed, "error")) {
    non_boot <- Filter(function(expr) {
      if (is.call(expr) && identical(as.character(expr[[1L]]), "if")) {
        cond_text <- paste(deparse(expr[[2L]]), collapse = " ")
        if (grepl(".sas2r_registry", cond_text, fixed = TRUE)) {
          return(FALSE)
        }
      }
      TRUE
    }, as.list(parsed))
    code_for_lint <- paste(vapply(non_boot, function(e) paste(deparse(e), collapse = "\n"), character(1)), collapse = "\n\n")
  }

  lint_res <- lint_r_code(code_for_lint)
  if (!is.null(helper_patch)) {
    helper_lint <- lint_helper_patch(helper_patch$content %||% "")
    if (nrow(helper_lint)) helper_lint$detail <- paste0(helper_patch$path %||% "candidate helpers", ": ", helper_lint$detail)
    lint_res <- rbind(lint_res, helper_lint)
  }
  if (is.data.frame(lint_res) && nrow(lint_res) > 0L) {
    lint_errors <- lint_res[lint_res$level == "error", , drop = FALSE]
    if (nrow(lint_errors) > 0L) {
      for (i in seq_len(nrow(lint_errors))) {
        errors <- c(errors, paste0("lint_error [", lint_errors$kind[i], "]: ", lint_errors$detail[i]))
      }
    }
    lint_warns <- lint_res[lint_res$level == "warn", , drop = FALSE]
    if (nrow(lint_warns) > 0L) {
      for (i in seq_len(nrow(lint_warns))) {
        warnings <- c(warnings, paste0("lint_warning [", lint_warns$kind[i], "]: ", lint_warns$detail[i]))
      }
    }
  }

  # 3. Helper name check
  if (!is.null(contract) && !is.null(contract$helper_use) && length(contract$helper_use) > 0L) {
    helpers_used <- reconcile_helper_use(code_text, contract$helper_use, contract$dependency_functions)
    invalid_helpers <- setdiff(helpers_used, c(SAS2R_HELPER_NAMES, contract$dependency_functions))
    if (length(invalid_helpers) > 0L) {
      errors <- c(errors, paste0("unknown_helper: ", paste(invalid_helpers, collapse = ", "),
        " (declared helper cannot be resolved; verify metadata and function availability)"))
    }
  }

  # 4. Declared interface check
  if (!is.null(contract) && (!is.null(contract$macro_contract) || length(contract$parameters) > 0L) && !inherits(parsed, "error")) {
    if (!is.null(contract$macro_contract)) {
      m_chk <- validate_macro_contract(code_text, contract$macro_contract)
      if (!isTRUE(m_chk$pass)) {
        errors <- c(errors, paste0("interface_error: ", m_chk$errors))
      }
      if (isTRUE(contract$macro_contract$standalone) && length(parsed) != 1L) {
        errors <- c(errors, "interface_error: a standalone macro file must contain only its function assignment; put executable behavior inside the function")
      }
    } else {
      # Script parameters can describe external inputs such as LIBNAME paths.
      # Compare formals only when this component defines its own named function;
      # an unrelated helper does not establish a callable program interface.
      param_names <- if (is.data.frame(contract$parameters)) {
        contract$parameters$name
      } else if (is.list(contract$parameters)) {
        vapply(contract$parameters, function(p) p$name %||% "", character(1))
      } else {
        character()
      }
      param_names <- param_names[nzchar(param_names)]

      if (length(param_names) > 0L) {
        all_fns <- Filter(function(expr) {
          is.call(expr) && length(expr) == 3L &&
            as.character(expr[[1L]]) %in% c("<-", "=") &&
            is.name(expr[[2L]]) &&
            is.call(expr[[3L]]) && identical(as.character(expr[[3L]][[1L]]), "function")
        }, as.list(parsed))

        matching_fn <- NULL
        if (!is.null(contract$component_id) && nzchar(contract$component_id)) {
          named_matches <- Filter(function(expr) identical(as.character(expr[[2L]]), contract$component_id), all_fns)
          if (length(named_matches) > 0L) {
            matching_fn <- named_matches[[1L]]
          }
        }
        if (!is.null(matching_fn)) {
          formals_names <- names(as.list(matching_fn[[3L]][[2L]])) %||% character()
          if (!identical(formals_names, param_names)) {
            errors <- c(errors, paste0("parameter_mismatch: expected (",
                                       paste(param_names, collapse = ", "),
                                       "), found (",
                                       paste(formals_names, collapse = ", "), ")"))
          }
        }
      }
    }
  }

  # A program can define macros AND run them. A definitions-only translation
  # loses that execution; standalone macro libraries intentionally only define.
  if (!inherits(parsed, "error") && !isTRUE(contract$macro_contract$standalone) &&
      nzchar(contract$sas_text %||% "")) {
    body <- smoke_program_expressions(code_text)
    definitions_only <- length(body) > 0L && all(vapply(body, function(expr) {
      is.call(expr) && length(expr) == 3L &&
        as.character(expr[[1L]])[1L] %in% c("<-", "=") &&
        is.name(expr[[2L]]) && is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))
    }, logical(1)))
    if (definitions_only) {
      units <- sas_units(sas_statements(contract$sas_text))
      calls <- extract_macro_calls(units[units$unit_type != "macro_def", , drop = FALSE])
      if (nrow(calls)) errors <- c(errors, paste0(
        "missing_program_execution: source invokes %", paste(unique(calls$name), collapse = ", %"),
        " outside macro definitions, but R only defines functions. Preserve the source invocation and its arguments/control flow."))
    }
  }

  # 5. Source / bootstrap check
  if (any(grepl("sas2r_source_include\\(", code_lines))) {
    inc_calls <- grep("sas2r_source_include\\(", code_lines, value = TRUE)
    for (ic in inc_calls) {
      if (grepl("sas2r_source_include\\(\\s*NA\\s*\\)", ic) || grepl("sas2r_source_include\\(\\s*\"\"\\s*\\)", ic)) {
        errors <- c(errors, "invalid_source_include: empty or NA target")
      }
    }
  }

  list(
    pass = length(errors) == 0L,
    errors = errors,
    warnings = warnings,
    lint = lint_res
  )
}
