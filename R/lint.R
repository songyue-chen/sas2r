#' Default names of sas2r runtime helper functions
#'
#' The complete set of names `inst/templates/sas2r-helpers.R` defines, and so
#' the complete set of names that may appear as a function call in a staged
#' bundle without [lint_r_code()] reporting an unknown function.
#'
#' It is deliberately the whole template surface rather than only the helpers a
#' generated unit calls. `sas2r_lib_entry()`, `sas2r_lib_member_path()`, and
#' `sas2r_libref_stop()` are called by the other helpers and not by emitted
#' unit code today; they still belong here, because the lint allowlist governs
#' every line of R in the bundle -- including a unit an agent authored or a
#' reviewer hand-edited, which may legitimately call them, and including the
#' helpers file itself. A list that tracked only what today's emitters happen
#' to call would turn every future template helper into a lint error the first
#' time anything used it.
#'
#' `tests/testthat/test-lint.R` pins the two as set-equal in both directions
#' for that reason: a name here that the template does not define is an
#' allowlist that permits a call nothing can satisfy, and a name the template
#' defines that is missing here is a call the linter will reject.
#'
#' @noRd
SAS2R_HELPER_NAMES <- c("%+%", "%notin%", "sas_sum", "sas_mean", "sas_round",
                        "sas_compress", "sas_substr", "sas_min", "sas_max",
                        "sas_length", "sas_put", "sas_sort", "sas_merge",
                        "sas_if_else", "sas2r_fold_names",
                        "apply_format", "lib_read", "lib_write", "chr_cmp",
                        "sas2r_source_include", "sas2r_libname_assign",
                        "sas2r_libname_clear", "sas2r_lib_entry",
                        "sas2r_lib_member_path", "sas2r_libref_stop",
                        "$.sas2r_dataset", "[[.sas2r_dataset", "split_ds",
                        "sas_display", "sas2r_registry_env")


BANNED_FUNCTIONS <- c("system", "system2", "shell", "download.file", "url",
                      "unlink", "file.remove", "Sys.setenv", "source",
                      "eval", "parse", "library", "require", "quit", "q")

find_isna_vars <- function(e) {
  vars <- character()
  if (!is.call(e)) return(vars)
  fn <- as.character(e[[1]])[1]
  if (fn == "(") {
    return(find_isna_vars(e[[2]]))
  }
  if (fn == "is.na" && length(e) >= 2L && is.name(e[[2]])) {
    return(as.character(e[[2]]))
  }
  if (fn == "!" && length(e) >= 2L) {
    return(find_isna_vars(e[[2]]))
  }
  if (fn %in% c("&", "|")) {
    for (i in seq_along(e)[-1]) vars <- c(vars, find_isna_vars(e[[i]]))
  }
  unique(vars)
}

#' Lint emitted or generated R code
#'
#' Scans R code for parse failures, banned functions, disallowed namespaces,
#' unknown functions, and unwrapped comparisons (missingness audits).
#'
#' @param code Character string or vector of R code.
#' @param allowlist Character vector of permitted package namespace names.
#' @param helpers Character vector of permitted helper function names.
#' @return A tibble with columns `level` ("error", "warn", or "info"),
#'   `kind`, and `detail`.
#' @noRd
# The bundle helpers accept exactly one call form each -- lib_read("lib",
# "member") and lib_write(df, "lib", "member") -- and fail loudly otherwise.
# Flagging the other forms statically turns a wrong call into repair feedback
# in the immediate loop, before a smoke run spends a subprocess on it. Only
# literal arguments can be judged; symbolic calls pass through to the runtime.
lib_call_misuse <- function(e, fname) {
  tryCatch({
    args <- as.list(e)[-1]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    canonical <- if (identical(fname, "lib_read")) {
      'lib_read("lib", "member")'
    } else {
      'lib_write(df, "lib", "member")'
    }
    is_str <- function(a) is.character(a) && length(a) == 1L
    if (any(nms %in% c("dataset", "table"))) {
      return(sprintf("%s(): dataset=/table= aliases are not accepted; use %s",
                     fname, canonical))
    }
    positional <- args[!nzchar(nms)]
    if (identical(fname, "lib_write") && length(positional) >= 1L && is_str(positional[[1L]])) {
      return(sprintf("%s() takes the data frame first; use %s", fname, canonical))
    }
    # Only the libref position can carry the combined form; a member literal
    # is validated (and path traversal refused) by the helper at runtime.
    libref_arg <- if ("libref" %in% nms) {
      args[["libref"]]
    } else {
      idx <- if (identical(fname, "lib_read")) 1L else 2L
      if (length(positional) >= idx) positional[[idx]] else NULL
    }
    if (!is.null(libref_arg) && is_str(libref_arg) && grepl(".", libref_arg, fixed = TRUE)) {
      return(sprintf('%s() does not accept combined "lib.member" references; use %s',
                     fname, canonical))
    }
    need <- if (identical(fname, "lib_read")) 2L else 3L
    if (length(args) < need) {
      return(sprintf("%s() needs both a libref and a member; use %s", fname, canonical))
    }
    NULL
  }, error = function(err) NULL)
}

lint_r_code <- function(code,
                        allowlist = c("base", "dplyr", "tidyr", "haven",
                                      "stats", "utils"),
                        helpers = SAS2R_HELPER_NAMES) {
  out <- list()
  add <- function(level, kind, detail) {
    out[[length(out) + 1L]] <<- tibble::tibble(
      level = as.character(level),
      kind = as.character(kind),
      detail = as.character(detail)
    )
  }

  exprs <- tryCatch(parse(text = code), error = function(e) e)
  if (inherits(exprs, "error")) {
    add("error", "parse_failure", conditionMessage(exprs))
    return(do.call(rbind, out))
  }

  base_fns <- c(ls(baseenv()), "|>", helpers)

  walk <- function(e, parent_isna_vars = character()) {
    if (!is.call(e)) return(invisible())
    fn <- e[[1]]
    fname <- if (is.name(fn)) {
      as.character(fn)
    } else if (is.call(fn) && as.character(fn[[1]])[1] %in% c("::", ":::")) {
      paste0(as.character(fn[[2]]), "::", as.character(fn[[3]]))
    } else {
      ""
    }
    plain <- sub("^.*::", "", fname)
    if (plain %in% BANNED_FUNCTIONS) {
      add("error", "banned_function", fname)
    }
    if (plain %in% c("lib_read", "lib_write")) {
      misuse <- lib_call_misuse(e, plain)
      if (!is.null(misuse)) add("error", "helper_misuse", misuse)
    }
    if (grepl("::", fname)) {
      pkg <- sub("::.*$", "", fname)
      if (!pkg %in% allowlist) {
        add("warn", "disallowed_namespace", fname)
      }
    } else if (nzchar(fname) && !fname %in% base_fns &&
               !fname %in% c("<-", "=", "(", "{", "[", "[[", "$", "~")) {
      add("info", "unknown_function", fname)
    }
    cur_isna_vars <- parent_isna_vars
    head_name <- if (nzchar(fname)) plain else as.character(fn)[1]
    if (head_name %in% c("&", "|")) {
      cur_isna_vars <- unique(c(parent_isna_vars, unlist(lapply(e[-1], find_isna_vars))))
    }
    if (plain %in% c("<", ">", "<=", ">=")) {
      v1 <- if (is.name(e[[2]])) as.character(e[[2]]) else NULL
      v2 <- if (length(e) > 2L && is.name(e[[3]])) as.character(e[[3]]) else NULL
      vars <- c(v1, v2)
      txt <- paste(deparse(e), collapse = " ")
      if (length(vars) > 0L && !any(vars %in% cur_isna_vars)) {
        add("warn", "unwrapped_comparison", txt)
      }
    }
    for (i in seq_along(e)[-1]) {
      # Test the extracted element in place. Binding an empty argument -- the
      # blank in x[, 1] or df[1, ] -- to a variable makes any later use of that
      # variable raise "argument is missing, with no default", so the emptiness
      # check has to happen before the binding, not after it.
      element <- tryCatch(
        if (identical(e[[i]], quote(expr = ))) NULL else list(e[[i]]),
        error = function(err) NULL
      )
      if (!is.null(element)) {
        walk(element[[1L]], parent_isna_vars = cur_isna_vars)
      }
    }
  }

  for (ex in exprs) walk(ex)
  if (!length(out)) {
    tibble::tibble(level = character(), kind = character(), detail = character())
  } else {
    do.call(rbind, out)
  }
}
