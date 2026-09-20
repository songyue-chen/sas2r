# Reuse the scanner's decision. This check only compares ordinary literal
# assignments; it does not interpret R control flow or resolve dynamic paths.
component_library_bindings <- function(project, component_id) {
  if (is.null(project$libref_registry)) return(NULL)
  statements <- component_statements(project, component_id)
  bindings <- effective_librefs(project)$bindings
  bindings[bindings$use_file %in% unique(statements$file), , drop = FALSE]
}

check_component_library_assignments <- function(code, project, component_id) {
  expressions <- tryCatch(parse(text = code), error = function(e) expression())
  if (!"sas2r_libname_assign" %in% all.names(expressions, unique = TRUE)) return(character())
  bindings <- component_library_bindings(project, component_id)
  if (is.null(bindings) || !nrow(bindings)) return(character())
  errors <- character()
  env <- new.env(parent = emptyenv())
  env$.sas2r_execution_root <- project$libref_registry$project_root
  visit <- function(expr) {
    if (!is.call(expr) && !is.expression(expr) && !is.pairlist(expr)) return()
    if (is.call(expr) && (identical(expr[[1L]], as.name("sas2r_libname_assign")) ||
        identical(expr[[1L]], quote(sas2r::sas2r_libname_assign)))) {
      call <- tryCatch(match.call(sas2r_libname_assign, expr), error = function(e) NULL)
      literal <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
      if (literal(call$libref) && literal(call$read_path)) {
        rows <- bindings[bindings$libref == tolower(call$libref), , drop = FALSE]
        # Unknown/conditional bindings cannot justify rejecting a path. Multiple
        # established paths remain valid: explicit source reassignments matter.
        if (nrow(rows) && all(rows$status == "bound") && all(!is.na(rows$selected_path))) {
          paths <- unique(vapply(rows$selected_path, sas2r_assignment_path, "", env = env))
          actual <- sas2r_assignment_path(call$read_path, env)
          if (!actual %in% paths) errors <<- c(errors, paste0(
            "lint_error [library_binding]: ", call$libref, " assignment resolves to ", actual,
            "; preflight selected ", paste(paths, collapse = " or "),
            ". Use the resolved LIBNAME plan, including configured fallbacks."))
        }
      }
    }
    for (i in seq_along(expr)) if (!identical(expr[[i]], quote(expr = ))) visit(expr[[i]])
  }
  visit(expressions)
  unique(errors)
}
