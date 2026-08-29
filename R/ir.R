#' Parse DATA step translation unit into intermediate representation
#'
#' @param stmts A tibble from [sas_units()] filtered to one DATA step `unit_id`.
#' @return A list of class `"sas2r_ir"` with elements:
#'   \item{outputs}{Character vector of normalized output dataset names.}
#'   \item{inputs}{Character vector of unique normalized input dataset names.}
#'   \item{route}{`"datastep"` or `"merge"`.}
#'   \item{by}{Character vector of by-variables (lowercased).}
#'   \item{in_flags}{Named character vector of dataset -> in= flag variable name (merge only).}
#'   \item{steps}{List of step records in original statement order.}
#'   \item{blockers}{A tibble with columns `stmt_id` and `reason`.}
#' @noRd
parse_data_step <- function(stmts) {
  code <- stmts[stmts$type == "code", ]
  steps <- list(); blockers <- list(); by <- character()
  inputs <- character(); outputs <- character(); in_flags <- character()
  route <- "datastep"

  blocker <- function(id, reason) {
    blockers[[length(blockers) + 1L]] <<- tibble::tibble(
      stmt_id = as.integer(id),
      reason = as.character(reason)
    )
  }
  step <- function(id, line, kind, ...) {
    steps[[length(steps) + 1L]] <<- c(
      list(kind = kind, stmt_id = as.integer(id), line = as.integer(line)),
      list(...)
    )
  }

  if (nrow(code) == 0L) {
    return(structure(
      list(outputs = character(), inputs = character(), route = route,
           by = by, in_flags = in_flags, steps = steps,
           blockers = tibble::tibble(stmt_id = integer(), reason = character())),
      class = "sas2r_ir"
    ))
  }

  txt_all <- paste(code$text, collapse = " ")
  masked_all <- mask_strings(txt_all, keep_double = TRUE)
  if (grepl("(%[A-Za-z_]|&[A-Za-z_])", masked_all)) {
    blocker(code$stmt_id[1], "macro_residue")
  }
  if (grepl("\\b(first|last)\\.|\\b_n_\\b", masked_all, ignore.case = TRUE)) {
    blocker(code$stmt_id[1], "by_group_logic")
  }

  for (k in seq_len(nrow(code))) {
    st <- code[k, ]; tok <- st$first_token; txt <- st$text; id <- st$stmt_id
    ln <- st$line_start
    if (tok == "data") {
      rest <- sub("^data(\\s+|$)", "", txt, ignore.case = TRUE)
      # ds_tokens() strips parenthesized option groups, so keep=/where=/rename=
      # on an output dataset would vanish without a trace. Options refuse until
      # they are modeled.
      if (grepl("\\(", rest)) blocker(id, "dataset_options")
      t <- ds_tokens(rest)
      outputs <- norm_ds(t[tolower(t) != "_null_"])
    } else if (tok == "set") {
      rest <- sub("^set(\\s+|$)", "", txt, ignore.case = TRUE)
      if (grepl("\\(", rest)) blocker(id, "dataset_options")
      t <- ds_tokens(rest)
      if (length(t) > 1L || length(inputs) > 0L) {
        blocker(id, "multi_set")
      } else {
        inputs <- norm_ds(t)
      }
    } else if (tok == "merge") {
      route <- "merge"
      groups <- regmatches(txt, gregexpr("\\([^)]*\\)", txt))[[1]]
      bad_groups <- groups[!grepl(
        "^\\(\\s*in\\s*=\\s*[A-Za-z_][A-Za-z0-9_]*\\s*\\)$", groups,
        ignore.case = TRUE)]
      if (length(bad_groups)) {
        # An option group beyond a bare (in=flag) would not only be dropped:
        # its tokens would be read back as extra input datasets.
        blocker(id, "merge_dataset_options")
        inputs <- c(inputs,
                    norm_ds(ds_tokens(sub("^merge(\\s+|$)", "", txt,
                                          ignore.case = TRUE))))
      } else {
        pieces <- regmatches(txt, gregexpr(
          "([A-Za-z_][A-Za-z0-9_.]*)[[:space:]]*([(][[:space:]]*in[[:space:]]*=[[:space:]]*([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*[)])?",
          txt, ignore.case = TRUE))[[1]][-1]
        for (p in pieces) {
          m <- regexec("([A-Za-z_][A-Za-z0-9_.]*)[[:space:]]*(?:[(][[:space:]]*in[[:space:]]*=[[:space:]]*([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*[)])?",
                       p, ignore.case = TRUE)
          g <- regmatches(p, m)[[1]]
          if (length(g) >= 2L && nzchar(g[2])) {
            ds <- norm_ds(g[2])
            inputs <- c(inputs, ds)
            nm <- split_ds(ds)[["member"]]
            in_flags[nm] <- if (length(g) >= 3L && nzchar(g[3])) tolower(g[3]) else NA_character_

          }
        }
      }
    } else if (tok == "by") {
      by <- tolower(strsplit(trimws(sub("^by(\\s+|$)", "", txt, ignore.case = TRUE)),
                             "\\s+")[[1]])
    } else if (tok == "where") {
      step(id, ln, "where", cond = trimws(sub("^where(\\s+|$)", "", txt, ignore.case = TRUE)))
    } else if (tok == "if") {
      m <- regmatches(txt, regexec(
        "^if\\s+(.*?)\\s+then\\s+(.*)$", txt, ignore.case = TRUE))[[1]]
      if (length(m) == 3L) {
        act <- trimws(m[3])
        if (tolower(act) == "delete") {
          step(id, ln, "if_delete", cond = trimws(m[2]))
        } else if (grepl("^[A-Za-z_]\\w*\\s*=", act)) {
          am <- regmatches(act, regexec("^([A-Za-z_]\\w*)\\s*=\\s*(.*)$", act))[[1]]
          step(id, ln, "if_assign", cond = trimws(m[2]), var = tolower(am[2]), expr = am[3])
        } else {
          blocker(id, "unsupported_statement:if_then_action")
        }
      } else if (route == "merge") {
        step(id, ln, "merge_filter",
             cond = trimws(sub("^if(\\s+|$)", "", txt, ignore.case = TRUE)))
      } else {
        blocker(id, "unsupported_statement:if_subsetting")
      }
    } else if (tok == "keep") {
      step(id, ln, "keep", vars = tolower(strsplit(trimws(
        sub("^keep(\\s+|$)", "", txt, ignore.case = TRUE)), "\\s+")[[1]]))
    } else if (tok == "drop") {
      step(id, ln, "drop", vars = tolower(strsplit(trimws(
        sub("^drop(\\s+|$)", "", txt, ignore.case = TRUE)), "\\s+")[[1]]))
    } else if (tok == "rename") {
      prs <- regmatches(txt, gregexpr("([A-Za-z_]\\w*)\\s*=\\s*([A-Za-z_]\\w*)",
                                      txt))[[1]]
      if (length(prs) == 0L) {
        pairs <- matrix(character(0), nrow = 0L, ncol = 2L,
                        dimnames = list(NULL, c("old", "new")))
      } else {
        pairs <- do.call(rbind, lapply(prs, function(p) {
          g <- regmatches(p, regexec("([A-Za-z_]\\w*)\\s*=\\s*([A-Za-z_]\\w*)", p))[[1]]
          c(old = tolower(g[2]), new = tolower(g[3]))
        }))
      }
      step(id, ln, "rename", pairs = pairs)
    } else if (tok %in% c("retain")) {
      blocker(id, "retain")
    } else if (tok == "output") {
      blocker(id, "output_statement")
    } else if (tok == "update") {
      blocker(id, "update_statement")
    } else if (grepl("^[A-Za-z_]\\w*$", tok) &&
               grepl("^\\s*[A-Za-z_]\\w*\\s*=", txt)) {
      am <- regmatches(txt, regexec("^\\s*([A-Za-z_]\\w*)\\s*=\\s*(.*)$", txt))[[1]]
      step(id, ln, "assign", var = tolower(am[2]), expr = am[3])
    } else if (tok %in% c("run", "quit")) {
      # end of unit
    } else {
      blocker(id, paste0("unsupported_statement:", tok))
    }
  }

  if (length(outputs) == 0L && length(blockers) == 0L) {
    blocker(code$stmt_id[1], "null_step_deferred")
  }
  if (length(inputs) == 0L && length(blockers) == 0L) {
    blocker(code$stmt_id[1], "no_input_dataset")
  }

  structure(
    list(outputs = outputs, inputs = unique(inputs), route = route,
         by = by, in_flags = in_flags, steps = steps,
         blockers = if (length(blockers)) do.call(rbind, blockers) else
           tibble::tibble(stmt_id = integer(), reason = character())),
    class = "sas2r_ir"
  )
}
