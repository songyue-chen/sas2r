KIND_CHAR_RE <- "('([^']|'')*'|\"([^\"]|\"\")*\")"

#' Infer dataset schemas and taint from code evidence
#'
#' @param project A `sas2r_project` object.
#' @return A named list keyed by dataset (e.g. `"work.x"`, `"w.root"`), where
#'   each entry is a list with:
#'   \item{vars}{A tibble with columns `var`, `kind` (`"numeric"` | `"character"`),
#'     and `source` (`"propagated"` | `"code"` | `"inferred"`).}
#'   \item{taint}{`"code"` or `"inferred"`.}
#' @noRd
infer_schemas <- function(project) {
  st <- project$statements
  evidence <- list()  # dataset -> var -> kind
  note <- function(ds, var, kind) {
    ds <- tolower(ds); var <- tolower(var)
    if (is.null(evidence[[ds]])) evidence[[ds]] <<- list()
    if (is.null(evidence[[ds]][[var]])) evidence[[ds]][[var]] <<- kind
  }
  units <- unique(st$unit_id[st$unit_type == "data_step"])
  irs <- list()
  for (uid in units) {
    us <- st[st$unit_id == uid, ]
    ir <- tryCatch(parse_data_step(us), error = function(e) NULL)
    if (is.null(ir) || !length(ir$inputs) || !length(ir$outputs)) next
    irs[[as.character(uid)]] <- ir
    src <- ir$inputs[1]
    for (s in ir$steps) {
      if (s$kind %in% c("where", "if_delete", "if_assign")) {
        cond_lc <- tolower(s$cond)
        # An untranslatable condition -- unresolved macro variables such as
        # `&m_var < &lo_var` are ordinary in real SAS -- yields no inferable
        # variables. Aborting here would take schema inference, and with it
        # verification, down for every unit in the project.
        cond_vars <- tryCatch(expr_vars(s$cond),
                              sas2r_expr_parse_error = function(e) character())
        for (v in cond_vars) {
          is_char <- grepl(paste0("\\b", v, "\\s*(=|eq|ne|\\^=|~=|<|<=|>|>=|lt|le|gt|ge)\\s*", KIND_CHAR_RE),
                           cond_lc, perl = TRUE) ||
                     grepl(paste0(KIND_CHAR_RE, "\\s*(=|eq|ne|\\^=|~=|<|<=|>|>=|lt|le|gt|ge)\\s*\\b", v, "\\b"),
                           cond_lc, perl = TRUE) ||
                     grepl(paste0("\\b", v, "\\s+(not\\s+in|in|%in%|%notin%)\\s*\\(\\s*", KIND_CHAR_RE),
                           cond_lc, perl = TRUE) ||
                     grepl(paste0("(\\b", v, "\\s*(\\|\\||%\\+%)|(\\|\\||%\\+%)\\s*\\b", v, "\\b)"),
                           cond_lc, perl = TRUE)
          note(src, v, if (is_char) "character" else "numeric")
        }
      }
      if (s$kind %in% c("assign", "if_assign") && !is.null(s$expr)) {
        expr_lc <- tolower(s$expr)
        is_char_assign <- grepl(paste0("^\\s*", KIND_CHAR_RE, "\\s*$"), s$expr, perl = TRUE) ||
                          grepl(paste0("(\\|\\||%\\+%|\\b(substr|compress|trim|strip|lowcase|upcase|scan|put)\\s*\\()"),
                                expr_lc, perl = TRUE)
        note(ir$outputs[1], s$var, if (is_char_assign) "character" else "numeric")
        expr_ast <- tryCatch(parse(text = tidy_expr(translate_expr(s$expr)))[[1]],
                             error = function(e) NULL)
        if (!is.null(expr_ast)) {
          for (v in setdiff(all.vars(expr_ast), s$var)) {
            # if used in concatenation or string functions, note as character
            if (grepl(paste0("(\\b", v, "\\s*(\\|\\||%\\+%)|(\\|\\||%\\+%)\\s*\\b", v, "\\b|\\b(substr|compress|trim|strip|lowcase|upcase|scan)\\s*\\([^)]*\\b", v, "\\b)"),
                      expr_lc, perl = TRUE)) {
              note(src, v, "character")
            } else {
              note(src, v, "numeric")
            }
          }
        }
      }
    }
  }
  # v1 limitation (visible, not hidden): only data-step-created datasets get
  # schemas. Datasets created by PROCs have none, so their consumers cap at
  # parsed/no_input_schema in the ladder. Sort-identity propagation is a
  # known follow-up.
  creators <- project$lineage[project$lineage$role == "creates", ]
  created <- unique(creators$dataset)
  all_ds <- unique(project$lineage$dataset)
  schemas <- list()
  ds_order <- c(setdiff(all_ds, created),
                created[order(match(
                  creators$unit_id[match(created, creators$dataset)],
                  project$order))])
  for (ds in ds_order) {
    is_root <- !ds %in% created
    vars <- evidence[[ds]]
    rows <- list(); taint <- if (is_root) "inferred" else "code"
    # propagate from input schema for created datasets
    if (!is_root) {
      uid <- project$lineage$unit_id[project$lineage$dataset == ds &
                                     project$lineage$role == "creates"][1]
      ir <- irs[[as.character(uid)]]
      if (!is.null(ir) && length(ir$inputs)) {
        up <- schemas[[ir$inputs[1]]]
        if (!is.null(up)) {
          taint <- up$taint
          keep <- Filter(function(s) s$kind == "keep", ir$steps)
          drop <- Filter(function(s) s$kind == "drop", ir$steps)
          rename <- Filter(function(s) s$kind == "rename", ir$steps)
          upv <- up$vars
          if (length(keep)) upv <- upv[upv$var %in% keep[[1]]$vars, ]
          if (length(drop)) upv <- upv[!upv$var %in% drop[[1]]$vars, ]
          if (length(rename) && nrow(rename[[1]]$pairs) > 0L) {
            pairs <- rename[[1]]$pairs
            for (p_idx in seq_len(nrow(pairs))) {
              old_nm <- pairs[p_idx, "old"]
              new_nm <- pairs[p_idx, "new"]
              upv$var[upv$var == old_nm] <- new_nm
            }
          }
          for (k in seq_len(nrow(upv))) {
            rows[[length(rows) + 1L]] <- tibble::tibble(
              var = upv$var[k], kind = upv$kind[k], source = "propagated")
          }
        } else taint <- "inferred"
      }
    }
    for (v in names(vars)) {
      if (!v %in% vapply(rows, function(r) r$var, character(1))) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          var = v, kind = vars[[v]],
          source = if (is_root) "inferred" else "code")
      }
    }
    schemas[[ds]] <- list(
      vars = if (length(rows)) do.call(rbind, rows) else
        tibble::tibble(var = character(), kind = character(), source = character()),
      taint = taint)
  }
  schemas
}
