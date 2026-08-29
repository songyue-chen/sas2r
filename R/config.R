KNOWN_CONFIG_KEYS <- c("llm", "libraries", "macros", "tolerance",
                       "search_docs", "privacy", "budget", "dialect",
                       "allowlist", "project", "includes", "environment",
                       "verification", "outputs", "comparison_rules",
                       "migration")

is_abs_path <- function(p) {
  grepl("^([A-Za-z]:)?[/\\\\]", p)
}

#' Test whether a configured path already carries its own base
#'
#' A resolution base is only ever prefixed onto a path that lacks one. An
#' absolute path carries its base outright; a `~`-rooted one carries it too,
#' because R expands `~` against the user's home directory rather than the
#' working directory, so prefixing either would corrupt the path rather than
#' anchor it.
#'
#' @param p Character vector of paths; must contain no `NA`.
#' @return Logical vector, `TRUE` where `p` needs no base.
#' @noRd
is_anchored_path <- function(p) {
  is_abs_path(p) | startsWith(p, "~")
}

#' Resolve configured paths against a fixed base, never the working directory
#'
#' A relative path in configuration is meaningless until something says what it
#' is relative to, and [normalizePath()] answers that question with
#' [getwd()] -- machine and invocation state, not project content. Since
#' `includes.roots` reaches occurrence identity through the anchor frame of
#' [include_identity_anchors()], letting `getwd()` answer it would make the same
#' bytes mint different ids for a developer in the repo root and for CI
#' elsewhere. So the base is supplied explicitly, once, at normalization time:
#' the configuration file's own directory when the configuration came from a
#' file, and the project root otherwise. Both are facts about the project's
#' layout, so both travel with the checkout.
#'
#' Empty and `NA` entries are left exactly as they are: they name no path, so
#' there is nothing to resolve, and the identity frame drops them anyway.
#'
#' @param paths Character vector of configured paths, in configured order.
#' @param base Directory the relative entries are relative to.
#' @return `paths`, with every relative entry prefixed by `base`.
#' @noRd
config_resolve_paths <- function(paths, base) {
  paths <- as.character(paths %||% character())
  if (!length(paths)) return(paths)
  usable <- !is.na(paths) & nzchar(paths)
  relative <- usable
  relative[usable] <- !is_anchored_path(paths[usable])
  paths[relative] <- file.path(base, paths[relative])
  paths
}

#' Rebase one configured path list onto the configuration file's directory
#'
#' The single rule every configured path group follows: `includes.roots`,
#' `macros.search_path`, and `environment.autoexec` alike. The base is
#' canonicalized first, so a relative `path=` or `SAS2R_CONFIG` spelling cannot
#' leave it relative and hand the question back to [getwd()]; a joined entry is
#' then normalized so the literal `..` between the base and the configured
#' spelling (`.../proj/../shared`) is gone by the time anyone prints or reads
#' it. Harmless either way -- the anchor builder and [file.exists()] both
#' normalize again -- but it is what a human sees.
#'
#' An already-anchored entry (absolute, or `~`-rooted) is left exactly as
#' configured, so a literal `/org/shared` comes back as `/org/shared`.
#'
#' @param paths Character vector of configured paths, in configured order.
#' @param src Configuration file path, or `NA` when there is no file.
#' @return `paths`, resolved against the configuration file's directory.
#' @noRd
config_rebase_paths <- function(paths, src) {
  paths <- as.character(paths %||% character())
  if (!length(paths) || length(src) != 1L || is.na(src)) return(paths)
  joined <- !is.na(paths) & nzchar(paths) & !is_anchored_path(paths)
  paths <- config_resolve_paths(paths, include_normalize_path(dirname(src)))
  paths[joined] <- include_normalize_path(paths[joined])
  paths
}

#' The dataset formats `lib_write()` can write
#'
#' The runtime half of this list lives in `inst/templates/sas2r-helpers.R`,
#' which stops with "Unsupported write format" for anything else. Keep the two
#' together: [normalize_library_entries()] is the check that turns that
#' run-time stop into a configuration-time one.
#' @noRd
LIBRARY_WRITE_FORMATS <- c("rds", "xpt")

# The member formats lib_read() can resolve. An engine outside this list used
# to normalize cleanly and silently do nothing at run time.
LIBRARY_READ_ENGINES <- c("sas7bdat", "xpt", "rds")

#' Normalize configured library entries against an explicit base
#'
#' A beginner writes `adam: data/adam`; an expert writes the three-field
#' mapping. Both mean the same thing, so both normalize to the same record --
#' `path`, `engine`, `write` -- and nothing downstream has to know which
#' spelling was used. The libref itself is lower-cased, because SAS librefs are
#' case-insensitive and two spellings of one libref would otherwise become two
#' libraries.
#'
#' The base is passed in rather than discovered: a relative path in
#' configuration is meaningless until something says what it is relative to,
#' and [normalizePath()] would answer that with [getwd()] -- machine and
#' invocation state, not project content.
#'
#' `write` is checked here rather than trusted, because nothing downstream can
#' check it: it is normalized, emitted into the generated registry, and first
#' read by `lib_write()` inside the bundle -- at run time, in whatever session
#' the user finally runs the translation in, after a reviewer has already read
#' the code. A format the runtime cannot honour is a configuration mistake, and
#' the place to report a configuration mistake is when the configuration is
#' read.
#'
#' @param raw Raw `libraries` mapping from configuration, or `NULL`.
#' @param base Directory relative entries are relative to.
#' @return A named list of normalized entries, `list()` when there are none.
#' @noRd
normalize_library_entries <- function(raw, base) {
  if (is.null(raw) || !length(raw)) return(list())
  librefs <- names(raw)
  if (!is.list(raw) || is.null(librefs) || anyNA(librefs) ||
      !all(nzchar(librefs))) {
    cli::cli_abort("{.field libraries} must be a mapping of libref to library",
                   class = "sas2r_config_error")
  }
  out <- lapply(librefs, function(libref) {
    value <- raw[[libref]]
    entry <- if (is.character(value) && length(value) == 1L) {
      list(path = value, engine = "sas7bdat", write = "rds")
    } else {
      value
    }
    if (!is.list(entry) || !is_scalar_character(entry$path)) {
      cli::cli_abort("library {.val {libref}} needs one path",
                     class = "sas2r_config_error")
    }
    path <- if (is_anchored_path(entry$path)) entry$path else
      file.path(base, entry$path)
    write <- tolower(entry$write %||% "rds")
    if (!is_scalar_character(write) || !(write %in% LIBRARY_WRITE_FORMATS)) {
      cli::cli_abort(
        c("library {.val {libref}} names a {.field write} format sas2r cannot write.",
          "x" = "Got {.val {entry$write}}.",
          "i" = "Supported: {.val {LIBRARY_WRITE_FORMATS}}."),
        class = "sas2r_config_error"
      )
    }
    engine <- tolower(entry$engine %||% "sas7bdat")
    if (!is_scalar_character(engine) || !(engine %in% LIBRARY_READ_ENGINES)) {
      cli::cli_abort(
        c("library {.val {libref}} names an {.field engine} sas2r cannot read.",
          "x" = "Got {.val {entry$engine}}.",
          "i" = "Supported: {.val {LIBRARY_READ_ENGINES}}."),
        class = "sas2r_config_error"
      )
    }
    list(path = normalizePath(path, winslash = "/", mustWork = FALSE),
         engine = engine,
         write = write)
  })
  names(out) <- tolower(librefs)
  duplicated_librefs <- unique(names(out)[duplicated(names(out))])
  if (length(duplicated_librefs)) {
    cli::cli_abort(
      c("Configured {.field libraries} names the same libref more than once.",
        "x" = "Repeated: {.val {duplicated_librefs}}.",
        "i" = "SAS librefs are case-insensitive; list each libref once."),
      class = "sas2r_config_error"
    )
  }
  out
}

#' Normalize configured libraries against the configuration file's directory
#'
#' The configuration-file form of [normalize_library_entries()]. `sas_project()`
#' uses the entry-level function directly with the project root as its base,
#' because a caller-supplied configuration list has no file to be relative to.
#'
#' The base is the configuration file's directory in *canonical* form, exactly
#' as [config_rebase_paths()] resolves it for `includes.roots`,
#' `macros.search_path`, and `environment.autoexec`. The two spellings agree
#' for every path that exists, because [normalizePath()] canonicalizes whatever
#' prefix does; they diverge only for a configured library that is not there
#' yet, and letting them diverge would mean one directory came back spelled two
#' ways depending on which configured group named it -- a difference a consumer
#' joining on `selected_path` would see as two libraries.
#'
#' A `libraries` mapping only ever arrives from a configuration file that was
#' read, so there is always a file to be relative to. Falling back to
#' [getwd()] when there is not would put invocation state -- neither project
#' content nor configuration -- back into a configured path, so the missing
#' base is reported instead.
#'
#' @param raw Raw `libraries` mapping from configuration, or `NULL`.
#' @param config_file Configuration file path.
#' @return A named list of normalized entries.
#' @noRd
normalize_library_config <- function(raw, config_file) {
  if (is.null(raw)) return(list())
  if (length(config_file) != 1L || is.na(config_file)) {
    cli::cli_abort(
      c("{.field libraries} needs the configuration file its paths are relative to.",
        "i" = "Pass entries with an explicit base to {.fn normalize_library_entries}."),
      class = "sas2r_config_error"
    )
  }
  normalize_library_entries(raw, include_normalize_path(dirname(config_file)))
}

normalize_budget_config <- function(config) {
  if (is.null(config)) return(list())
  if (!is.list(config)) {
    cli::cli_abort("budget configuration must be a mapping",
                   class = "sas2r_budget_config_error")
  }
  allowed <- c(
    "mode", "max_usd", "pricing_source", "rates", "max_calls",
    "max_retries", "max_tool_calls", "max_wall_time",
    "max_request_bytes", "max_request_chars", "max_input_tokens",
    "max_output_tokens"
  )
  unknown <- setdiff(names(config), allowed)
  if (length(unknown)) {
    cli::cli_abort(
      "unknown budget configuration field{?s}: {.field {unknown}}",
      class = "sas2r_budget_config_error"
    )
  }
  validated <- do.call(new_usage_budget, config)
  out <- config
  if (!is.null(out$mode)) out$mode <- validated$mode
  if (!is.null(out$pricing_source)) {
    out$pricing_source <- validated$pricing_source
  }
  out
}

assert_exact_names <- function(x, allowed, context = "config") {
  if (!is.list(x)) {
    cli::cli_abort("{.field {context}} must be a mapping",
                   class = "sas2r_config_error")
  }
  unknown <- setdiff(names(x), allowed)
  if (length(unknown)) {
    cli::cli_abort(
      c("Unknown or unsupported field in {.field {context}}: {.val {unknown}}",
        "i" = "Allowed: {.val {allowed}}."),
      class = "sas2r_config_error"
    )
  }
}

normalize_named_paths <- function(raw_paths, config_file) {
  if (is.null(raw_paths) || length(raw_paths) == 0L) return(list())
  if (!is.list(raw_paths) || is.null(names(raw_paths)) || anyNA(names(raw_paths)) ||
      !all(nzchar(names(raw_paths)))) {
    cli::cli_abort("r_libraries must be a mapping of library names to paths",
                   class = "sas2r_config_error")
  }
  base <- if (!is.na(config_file)) include_normalize_path(dirname(config_file)) else getwd()
  out <- list()
  for (nm in names(raw_paths)) {
    val <- raw_paths[[nm]]
    if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
      cli::cli_abort("library {.val {nm}} must specify a single non-empty path string",
                     class = "sas2r_config_error")
    }
    path <- if (is_anchored_path(val)) val else file.path(base, val)
    out[[tolower(nm)]] <- normalizePath(path, winslash = "/", mustWork = FALSE)
  }
  duplicated_names <- unique(names(out)[duplicated(names(out))])
  if (length(duplicated_names)) {
    cli::cli_abort(
      c("Configured library names must be unique.",
        "x" = "Repeated: {.val {duplicated_names}}."),
      class = "sas2r_config_error"
    )
  }
  out
}

normalize_output_review_config <- function(raw, config_file) {
  if (is.null(raw) || is.null(raw$verification)) {
    return(list(enabled = FALSE, r_libraries = list()))
  }
  if (!is.list(raw$verification)) {
    cli::cli_abort("verification configuration must be a mapping",
                   class = "sas2r_config_error")
  }
  assert_exact_names(raw$verification, "output_review", context = "verification")
  value <- raw$verification$output_review
  if (is.null(value)) {
    return(list(enabled = FALSE, r_libraries = list()))
  }
  if (!is.list(value)) {
    cli::cli_abort("verification.output_review must be a mapping",
                   class = "sas2r_config_error")
  }
  assert_exact_names(value, c("enabled", "r_libraries"),
                     context = "verification.output_review")
  enabled <- value$enabled %||% FALSE
  if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
    cli::cli_abort("output_review.enabled must be true or false",
                   class = "sas2r_config_error")
  }
  roots <- normalize_named_paths(value$r_libraries %||% list(), config_file)
  if (enabled && !length(roots)) {
    cli::cli_abort("enabled output review needs at least one r_libraries root",
                   class = "sas2r_config_error")
  }
  list(enabled = enabled, r_libraries = roots)
}

normalize_outputs_config <- function(raw_outputs, config_file = NA_character_) {
  if (is.null(raw_outputs)) return(NULL)
  validate_output_overrides(raw_outputs)
}

find_config <- function(start = ".") {
  env <- Sys.getenv("SAS2R_CONFIG", unset = "")
  if (nzchar(env)) return(env)
  dir <- normalizePath(start, mustWork = FALSE)
  repeat {
    candidate <- file.path(dir, "_sas2r.yml")
    if (file.exists(candidate)) return(candidate)
    parent <- dirname(dir)
    if (identical(parent, dir)) return(NA_character_)
    dir <- parent
  }
}

#' Load project configuration (optional by design)
#'
#' Looks for `_sas2r.yml` upward from `start`; the `SAS2R_CONFIG`
#' environment variable overrides discovery. No config file is the
#' primary supported case: built-in defaults let a bare SAS script be
#' scanned, assessed, and translated with no setup at all.
#' Every relative configured path -- `libraries`, `macros.search_path`,
#' `includes.roots`, and `environment.autoexec` -- is resolved against the
#' configuration file's own directory, never against the working directory:
#' those roots reach `%include` occurrence identity, which must not depend on
#' where the scan was launched from. One anchoring rule governs all four, so a
#' `~`-prefixed entry is treated as already carrying its own base everywhere
#' rather than in some keys only, because R expands `~` against the user's home
#' directory and prefixing a base onto it would corrupt the path.
#'
#' A caller-supplied configuration list has no file of its own, so
#' `sas_project()` resolves whatever is still relative against the project
#' root. Both bases are facts about the project's layout, and both travel with
#' the checkout.
#' @param path Path to a configuration file. Defaults to `NULL` (use discovery).
#' @param start Directory from which to search upwards for `_sas2r.yml`. Defaults to `"."`.
#' @return A `sas2r_config` object containing `libraries`, `macro_search_path`,
#'   `include_roots`, optional normalized `llm`, `output_review`, `outputs`, `source`, and `raw`.
#' @export
sas_config <- function(path = NULL, start = ".") {
  src <- if (!is.null(path)) path else find_config(start)
  raw <- list()
  if (!is.na(src)) {
    raw <- yaml::read_yaml(src)
    if (is.null(raw)) raw <- list()
    unknown <- setdiff(names(raw), KNOWN_CONFIG_KEYS)
    if (length(unknown)) {
      msg <- paste0("Unknown config key", if (length(unknown) > 1) "s" else "", " in {.file {src}}: {.val {unknown}}")
      cli::cli_warn(msg)
    }
  }
  llm <- if (is.null(raw$llm)) NULL else normalize_llm_config(raw$llm)
  budget <- normalize_budget_config(raw$budget)
  output_review <- normalize_output_review_config(raw, src)
  outputs <- normalize_outputs_config(raw$outputs, src)
  include_roots <- if (is.null(raw$includes$roots)) character()
                   else as.character(raw$includes$roots)
  macro_search_path <- if (is.character(raw$macros)) raw$macros
                       else if (is.list(raw$macros) && !is.null(raw$macros$search_path)) as.character(raw$macros$search_path)
                       else character()
  autoexec <- if (is.null(raw$environment$autoexec)) character()
              else as.character(raw$environment$autoexec)
  structure(list(
    libraries = normalize_library_config(raw$libraries, src),
    macro_search_path = config_rebase_paths(macro_search_path, src),
    include_roots = config_rebase_paths(include_roots, src),
    autoexec = config_rebase_paths(autoexec, src),
    outputs = outputs,
    output_review = output_review,
    comparison_rules = raw$comparison_rules %||% list(),
    llm = llm,
    budget = budget,
    source = src,
    raw = raw
  ), class = "sas2r_config")
}

#' @export
print.sas2r_config <- function(x, ...) {
  cli::cli_h1("sas2r configuration")
  if (is.na(x$source)) cli::cli_text("source: {.emph built-in defaults (no _sas2r.yml found)}")
  else cli::cli_text("source: {.file {x$source}}")
  cli::cli_text("libraries: {length(x$libraries)}")
  cli::cli_text("macro search path: {length(x$macro_search_path)} location{?s}")
  invisible(x)
}
