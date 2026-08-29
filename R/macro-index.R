MACRO_INDEX_SCHEMA_VERSION <- "1.0"

#' Build macro definition index over directories
#'
#' Scans every `.sas` file under `dirs` (non-recursive per configured root,
#' matching autocall semantics), extracts `%macro` definitions with
#' `extract_macro_defs()`, cached per file by content hash in
#' `cache_dir/macro_index.rds`.
#'
#' @param dirs Character vector of directories to scan.
#' @param cache_dir Optional directory path where `macro_index.rds` is cached.
#' @return A tibble with columns `name`, `file`, `line_start`, `line_end`.
#' @noRd
build_macro_index <- function(dirs, cache_dir = NULL) {
  cache_file <- if (!is.null(cache_dir)) file.path(cache_dir, "macro_index.rds")
  cache <- if (!is.null(cache_file) && file.exists(cache_file)) {
    tryCatch(readRDS(cache_file), error = function(e) list())
  } else {
    list()
  }
  if (!is.list(cache)) cache <- list()
  rows <- list()
  valid_dirs <- dirs[dir.exists(dirs)]
  cache_dirty <- FALSE

  for (d in valid_dirs) {
    for (f in list.files(d, pattern = "\\.sas$", full.names = TRUE,
                         ignore.case = TRUE)) {
      raw_text <- paste(readLines(f, warn = FALSE), collapse = "\n")
      key <- paste0("v", MACRO_INDEX_SCHEMA_VERSION, "_",
                    normalizePath(f, mustWork = FALSE), "_", cli::hash_md5(raw_text))
      hit <- cache[[key]]
      if (is.null(hit)) {
        u <- sas_units(sas_statements(raw_text))
        defs <- extract_macro_defs(u)
        hit <- if (nrow(defs) > 0L) {
          tibble::tibble(
            name = defs$name, file = f,
            line_start = defs$line_start, line_end = defs$line_end
          )
        } else {
          tibble::tibble(
            name = character(), file = character(),
            line_start = integer(), line_end = integer()
          )
        }
        cache[[key]] <- hit
        cache_dirty <- TRUE
      }
      rows[[length(rows) + 1L]] <- hit
    }
  }

  if (!is.null(cache_file) && cache_dirty) {
    tryCatch({
      atomic_write_file(function(tf) saveRDS(cache, tf), cache_file, pattern = "macro_idx_")
    }, error = function(e) {
      cli::cli_warn("cannot write macro index cache to {.file {cache_file}}: {conditionMessage(e)}",
                    class = "sas2r_cache_write_failed")
    })
  }

  fast_bind(rows, tibble::tibble(name = character(), file = character(),
                                 line_start = integer(), line_end = integer()))
}

#' The project's configured macro index, on the shared cache
#'
#' One recipe for every agent path: resolve `macro_search_path` against the
#' project directory and build (or reuse) the cached index. `NULL` when there
#' is no project or the index cannot be built -- the macro tools then answer
#' `not_in_index` instead of erroring.
#'
#' @param project A `sas2r_project`, or `NULL`.
#' @param config The project configuration list.
#' @return A macro index tibble, or `NULL`.
#' @noRd
project_macro_index <- function(project, config = list()) {
  if (is.null(project) || is.null(project$project_dir)) return(NULL)
  dirs <- if (length(config$macro_search_path)) {
    vapply(config$macro_search_path, function(d) {
      if (is_anchored_path(d)) d else file.path(project$project_dir, d)
    }, character(1), USE.NAMES = FALSE)
  } else {
    character()
  }
  tryCatch(
    build_macro_index(dirs, cache_dir = file.path(project$project_dir, ".sas2r")),
    error = function(e) NULL
  )
}
