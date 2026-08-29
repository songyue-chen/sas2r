#' Group statements into translation units
#'
#' Units are the atoms of translation: whole DATA steps, whole PROC steps,
#' whole macro definitions, and single global statements.
#' @param stmts A tibble from [sas_statements()].
#' @return The input tibble with unit_id and unit_type columns added.
#' @noRd
sas_units <- function(stmts) {
  n <- nrow(stmts)
  unit_id <- integer(n)
  unit_type <- character(n)
  cur_id <- 0L
  cur_type <- NA_character_
  open <- FALSE
  macro_depth <- 0L

  # Context stack for enclosing step when %macro is defined inside a step
  outer_id <- NA_integer_
  outer_type <- NA_character_
  outer_open <- FALSE

  just_closed_id <- NA_integer_
  just_closed_type <- NA_character_

  for (k in seq_len(n)) {
    tok <- tolower(stmts$first_token[k])

    if (macro_depth > 0L) {
      if (tok == "%macro") macro_depth <- macro_depth + 1L
      if (tok == "%mend") macro_depth <- macro_depth - 1L
      unit_id[k] <- cur_id
      unit_type[k] <- "macro_def"
      if (macro_depth == 0L) {
        if (outer_open) {
          open <- TRUE
          cur_id <- outer_id
          cur_type <- outer_type
          outer_open <- FALSE
        } else {
          open <- FALSE
          cur_type <- NA_character_
        }
      }
      next
    }

    if (tok == "%macro") {
      if (open) {
        outer_open <- TRUE
        outer_id <- cur_id
        outer_type <- cur_type
      }
      cur_id <- cur_id + 1L
      cur_type <- "macro_def"
      macro_depth <- 1L
      open <- TRUE
      unit_id[k] <- cur_id
      unit_type[k] <- "macro_def"
    } else if (tok %in% c("data", "proc")) {
      cur_id <- cur_id + 1L
      cur_type <- if (tok == "data") "data_step" else "proc_step"
      open <- TRUE
      unit_id[k] <- cur_id
      unit_type[k] <- cur_type
      just_closed_id <- NA_integer_
    } else if (tok %in% c("run", "quit")) {
      if (open) {
        unit_id[k] <- cur_id
        unit_type[k] <- cur_type
        open <- FALSE
        just_closed_id <- cur_id
        just_closed_type <- cur_type
      } else if (!is.na(just_closed_id) && k > 1L && tolower(stmts$first_token[k - 1L]) %in% c("run", "quit")) {
        # Consecutive run; quit; or quit; run; - keep with the preceding closed step
        unit_id[k] <- just_closed_id
        unit_type[k] <- just_closed_type
      } else {
        cur_id <- cur_id + 1L
        unit_id[k] <- cur_id
        unit_type[k] <- "global"
        just_closed_id <- NA_integer_
      }
    } else if (open) {
      unit_id[k] <- cur_id
      unit_type[k] <- cur_type
    } else {
      cur_id <- cur_id + 1L
      unit_id[k] <- cur_id
      unit_type[k] <- "global"
      just_closed_id <- NA_integer_
    }
  }

  stmts$unit_id <- unit_id
  stmts$unit_type <- unit_type
  tibble::as_tibble(stmts)
}

