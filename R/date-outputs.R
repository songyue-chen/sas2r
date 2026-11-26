# SAS dates in delivered datasets
#
# SAS stores a date as a number of days from 1960-01-01 plus a format that
# says to show the number as a date. Generated R keeps the number, so SAS date
# arithmetic translates unchanged. This file reads the formats from the SAS
# source and decides which output columns a bundle writes as R `Date`; the
# conversion itself is inside lib_write() and lib_read() in R/runtime-data.R,
# where generated code can neither call it nor replace it.
#
# A column is written as a date on two facts that do not depend on each other:
#   1. the SAS source gives the variable a date format, in a FORMAT or ATTRIB
#      statement this file reads exactly, on the way to that output; and
#   2. the SAS reference for the output stores the column as a date. Where no
#      reference column exists, the declaration must instead be followed to
#      the output through steps that are read exactly, with no macro-generated
#      code in between.
# A date-like name, a plausible day number, or a date column in a reference
# decides nothing alone. Anything else is left as the SAS number and listed
# with the reason, so nothing is typed on a guess.
#
# "On the way to that output" follows the variable, not its name: a DROP or
# KEEP that removes the variable removes the format with it, and a variable of
# the same name made afterwards is a new one. Where the source proves that, the
# first fact does not hold and the reference cannot supply it. "That output" is
# the dataset as the programs leave it, so a step that can change it after it
# is written ends the source-only trace too.

# The SAS format names haven reads back as `Date`. A reference dataset is read
# with haven, so a format typed here and not there would turn every such column
# into a type difference. tests/testthat/test-date-outputs.R holds the two
# lists together.
SAS_DATE_FORMAT_PATTERN <-
  "^(DATE|WEEKDATE|(YYMMDD|MMDDYY|DDMMYY)[BCDNPS]?|E8601DA|B8601DA|IS8601DA)[0-9]*$"

# Procedures that can change a dataset's variables in place, without naming it
# as an output the lineage records.
DATE_TRACE_MODIFYING_PROCS <- c("datasets", "sql", "append", "copy", "cimport",
                                "fedsql", "ds2", "iml", "delete")

#' The date format a SAS format token names, if it names one
#'
#' @param token One format token as written in SAS, such as `yymmdd10.`.
#' @return The upper-case format name without its dot, or `NA_character_`.
#' @noRd
sas_date_format_name <- function(token) {
  hit <- regmatches(token, regexec("^([A-Za-z_][A-Za-z0-9_]*)\\.$", token))[[1]]
  if (length(hit) != 2L) return(NA_character_)
  name <- toupper(hit[2])
  if (grepl(SAS_DATE_FORMAT_PATTERN, name)) name else NA_character_
}

is_sas_format_token <- function(token) grepl("^\\$?[A-Za-z0-9_]*\\.[0-9]*$", token)
is_sas_plain_name <- function(token) {
  grepl("^[A-Za-z_][A-Za-z0-9_]*$", token) &
    !tolower(token) %in% c("_all_", "_numeric_", "_character_", "_char_")
}

# Text that macro processing could change is never read as a declaration.
has_macro_trigger <- function(text) {
  grepl("[%&]", vapply(text, mask_strings, "", keep_double = TRUE, USE.NAMES = FALSE))
}

#' Read the format each variable gets from one FORMAT or ATTRIB statement
#'
#' @param text Statement text without its semicolon.
#' @return `NULL` when the statement cannot be read exactly: a macro reference,
#'   a variable list such as `a--b`, `x1-x5`, `dt:` or `_numeric_`, or any
#'   token outside the plain grammar. Otherwise a named list, one entry per
#'   variable the statement changes, each `list(kind, format)` with `kind`
#'   `"date"`, `"other"` or `"none"` (the format is removed).
#' @noRd
parse_format_declaration <- function(text) {
  if (has_macro_trigger(text)) return(NULL)
  masked <- mask_strings(text)
  masked <- gsub("'[^']*'|\"[^\"]*\"", " Q ", masked)
  keyword <- tolower(sub("^\\s*([A-Za-z]+).*$", "\\1", masked))
  body <- sub("^\\s*[A-Za-z]+", "", masked)
  if (grepl("[^A-Za-z0-9_$.=\\s]", body, perl = TRUE)) return(NULL)
  body <- gsub("\\s*=\\s*", "=", body)
  tokens <- strsplit(trimws(body), "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  out <- list()
  entry <- function(token) {
    name <- sas_date_format_name(token)
    if (is.na(name)) list(kind = "other", format = toupper(sub("\\.$", "", token)))
    else list(kind = "date", format = name)
  }
  if (identical(keyword, "format")) {
    pending <- character()
    for (token in tokens) {
      if (grepl("=", token, fixed = TRUE)) return(NULL)
      if (is_sas_format_token(token)) {
        if (!length(pending)) return(NULL)
        for (v in pending) out[[v]] <- entry(token)
        pending <- character()
      } else if (is_sas_plain_name(token)) {
        pending <- c(pending, tolower(token))
      } else return(NULL)
    }
    for (v in pending) out[[v]] <- list(kind = "none", format = NA_character_)
    return(out)
  }
  if (identical(keyword, "attrib")) {
    group <- character()
    closed <- FALSE
    for (token in tokens) {
      if (grepl("=", token, fixed = TRUE)) {
        parts <- strsplit(token, "=", fixed = TRUE)[[1]]
        if (length(parts) != 2L || !length(group)) return(NULL)
        key <- tolower(parts[1])
        if (!key %in% c("label", "format", "informat", "length", "transcode")) return(NULL)
        if (key == "format") {
          if (!is_sas_format_token(parts[2])) return(NULL)
          for (v in group) out[[v]] <- entry(parts[2])
        }
        closed <- TRUE
      } else if (is_sas_plain_name(token)) {
        if (closed) { group <- character(); closed <- FALSE }
        group <- c(group, tolower(token))
      } else return(NULL)
    }
    return(out)
  }
  NULL
}

#' Read the pairs of a RENAME statement
#'
#' @return A character matrix with columns `old` and `new`, or `NULL` when the
#'   statement cannot be read exactly.
#' @noRd
parse_rename_statement <- function(text) {
  if (has_macro_trigger(text)) return(NULL)
  body <- sub("^\\s*rename\\b", "", mask_strings(text), ignore.case = TRUE)
  if (grepl("[^A-Za-z0-9_=\\s]", body, perl = TRUE)) return(NULL)
  tokens <- strsplit(trimws(gsub("\\s*=\\s*", "=", body)), "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens) || !all(grepl("^[A-Za-z_][A-Za-z0-9_]*=[A-Za-z_][A-Za-z0-9_]*$", tokens))) return(NULL)
  pairs <- do.call(rbind, strsplit(tolower(tokens), "=", fixed = TRUE))
  colnames(pairs) <- c("old", "new")
  pairs
}

# DROP and KEEP. A format travels with its variable, so a variable that is
# dropped, or left out of a KEEP list, takes its format out of the dataset: a
# variable of the same name created later is a new one with no format.

#' Whether one token of a DROP or KEEP list selects a variable
#'
#' @return `TRUE`, `FALSE`, or `NA` when the token is not read exactly: a
#'   name range such as `a--b`, a keyword list such as `_numeric_`, or a
#'   numbered range that could hold the variable.
#' @noRd
projection_token_selects <- function(token, var) {
  token <- tolower(token)
  if (is_sas_plain_name(token)) return(identical(token, var))
  if (grepl("^[a-z_][a-z0-9_]*:$", token)) return(startsWith(var, sub(":$", "", token)))
  range <- regmatches(token, regexec("^([a-z_][a-z0-9_]*?)[0-9]+-([a-z_][a-z0-9_]*?)[0-9]+$", token))[[1]]
  if (length(range) == 3L && identical(range[2], range[3]) &&
      !grepl(paste0("^", range[2], "[0-9]+$"), var)) return(FALSE)
  NA
}

#' One DROP or KEEP list
#'
#' @param op `"drop"` or `"keep"`.
#' @param body The list as written, without its keyword.
#' @return `list(op, tokens, line)`.
#' @noRd
read_projection_list <- function(op, body, line) {
  tokens <- strsplit(trimws(gsub("\\s*-\\s*", "-", body)), "\\s+")[[1]]
  list(op = tolower(op), tokens = tokens[nzchar(tokens)], line = as.integer(line))
}

#' Whether a series of DROP and KEEP lists removes a variable
#'
#' The lists apply one after another, so one list that removes the variable
#' settles it whatever the others say.
#'
#' @return `list(excluded, op, line)`: `excluded` is `TRUE`, `FALSE`, or `NA`
#'   when no list removes the variable for certain and one could; `line` is
#'   the list that decided.
#' @noRd
projection_excludes <- function(filters, var) {
  unknown <- NA_integer_
  for (f in filters) {
    hits <- vapply(f$tokens, projection_token_selects, logical(1), var = var, USE.NAMES = FALSE)
    selected <- if (any(hits %in% TRUE)) TRUE else if (!length(hits) || anyNA(hits)) NA else FALSE
    gone <- if (f$op == "drop") selected else !selected
    if (isTRUE(gone)) return(list(excluded = TRUE, op = toupper(f$op), line = f$line))
    if (is.na(gone) && is.na(unknown)) unknown <- f$line
  }
  list(excluded = if (is.na(unknown)) FALSE else NA, op = NA_character_, line = unknown)
}

# Which tokens of a statement, as dataset_tokens() splits it, are outside every
# parenthesis.
top_level_tokens <- function(lex) {
  open <- lex == "("
  close <- lex == ")"
  (cumsum(open) - cumsum(close)) == 0L & !open & !close
}

#' The DROP= and KEEP= lists among one dataset's options
#'
#' @param lex A statement's tokens from [dataset_tokens()].
#' @param at The position of the dataset name; its options are the
#'   parenthesised group that follows.
#' @return A list of [read_projection_list()] records, empty when the dataset
#'   has no DROP= or KEEP= option. Text inside a nested option such as WHERE=
#'   or inside a quoted string is not read as an option.
#' @noRd
dataset_option_filters <- function(lex, at, line) {
  n <- length(lex)
  if (at >= n || lex[at + 1L] != "(") return(list())
  depth <- cumsum(lex == "(") - cumsum(lex == ")")
  end <- which(seq_len(n) > at + 1L & lex == ")" & depth == depth[at])[1L]
  unread <- list(list(op = "drop", tokens = character(), line = as.integer(line)))
  if (is.na(end)) return(unread)
  inner <- if (end > at + 2L) lex[seq.int(at + 2L, end - 1L)] else character()
  top <- top_level_tokens(inner)
  filters <- list()
  for (i in which(top & tolower(inner) %in% c("drop", "keep") & c(inner[-1L], "") == "=")) {
    j <- i + 2L
    while (j <= length(inner) && top[j] && !inner[j] %in% c("=", ",") &&
           !(j < length(inner) && inner[j + 1L] == "=")) j <- j + 1L
    # A list that runs into a parenthesis or a stray sign is not a plain list.
    if (j <= length(inner) && (!top[j] || inner[j] %in% c("=", ","))) return(unread)
    body <- if (j > i + 2L) paste(inner[seq.int(i + 2L, j - 1L)], collapse = " ") else ""
    filters[[length(filters) + 1L]] <- read_projection_list(inner[i], body, line)
  }
  filters
}

# The steps of one source file in the order they run, with what each reads and
# creates. A macro definition is text, not a step, and is left out.
date_trace_file <- function(project, file) {
  st <- project$statements
  st <- st[st$type == "code" & st$file == file & st$unit_type != "macro_def", , drop = FALSE]
  ids <- unique(st$unit_id[order(st$line_start, st$stmt_id)])
  lin <- project$lineage
  lin <- lin[lin$unit_id %in% ids, , drop = FALSE]
  other <- project$statements
  other <- other[other$type == "code" & other$file != file & other$unit_type != "macro_def", , drop = FALSE]
  found <- new.env(parent = emptyenv())
  list(
    ids = ids,
    # The steps of this file that run after step `id`.
    after = function(id) {
      a <- match(id, ids)
      if (a >= length(ids)) integer() else ids[seq.int(a + 1L, length(ids))]
    },
    # The steps of the other programs that name `member`, or that copy into
    # `library`. Their place in the run is not known here, so each may run
    # later.
    outside = function(member, library) {
      if (is.null(found$outside)) {
        has <- function(pattern) other$unit_id[grepl(pattern, other$text, ignore.case = TRUE, perl = TRUE)]
        hits <- unique(c(has(member), intersect(has("^\\s*proc\\s+(copy|cimport|datasets)\\b"), has(library))))
        found$outside <- lapply(hits, function(id) {
          us <- other[other$unit_id == id, , drop = FALSE]
          list(us = us[order(us$line_start, us$stmt_id), , drop = FALSE], type = us$unit_type[1L],
               reads = unique(project$lineage$dataset[project$lineage$unit_id == id & project$lineage$role == "reads"]))
        })
      }
      found$outside
    },
    statements = function(id) {
      us <- st[st$unit_id == id, , drop = FALSE]
      us[order(us$line_start, us$stmt_id), , drop = FALSE]
    },
    type = function(id) st$unit_type[match(id, st$unit_id)],
    reads = function(id) unique(lin$dataset[lin$unit_id == id & lin$role == "reads"]),
    creates = function(id) unique(lin$dataset[lin$unit_id == id & lin$role == "creates"]),
    # The step that last wrote `dataset` before step `id` runs.
    prior_writer = function(id, dataset) {
      before <- ids[seq_len(match(id, ids) - 1L)]
      hits <- before[before %in% lin$unit_id[lin$role == "creates" & lin$dataset == dataset]]
      if (length(hits)) hits[length(hits)] else NA_integer_
    },
    between = function(from, to) {
      a <- match(from, ids); b <- match(to, ids)
      if (b - a < 2L) integer() else ids[seq.int(a + 1L, b - 1L)]
    }
  )
}

# Every step that feeds the writer of an output, through the datasets each one
# reads. Dataset names only: this says where a declaration could reach, not
# that it does.
date_trace_ancestors <- function(trace, writer) {
  seen <- integer()
  queue <- writer
  while (length(queue)) {
    id <- queue[1L]; queue <- queue[-1L]
    if (id %in% seen) next
    seen <- c(seen, id)
    for (dataset in trace$reads(id)) {
      prior <- trace$prior_writer(id, dataset)
      if (!is.na(prior)) queue <- c(queue, prior)
    }
  }
  trace$ids[trace$ids %in% seen]
}

# Each FORMAT and ATTRIB statement in the given DATA steps, in running order.
date_declarations <- function(trace, units) {
  rows <- list()
  for (id in units) {
    if (!identical(trace$type(id), "data_step")) next
    us <- trace$statements(id)
    for (k in which(us$first_token %in% c("format", "attrib"))) {
      parsed <- parse_format_declaration(us$text[k])
      base <- list(unit_id = id, line = as.integer(us$line_start[k]), file = us$file[k],
                   text = trimws(gsub("\\s+", " ", us$text[k])))
      if (is.null(parsed)) {
        rows[[length(rows) + 1L]] <- c(base, list(var = NA_character_, kind = "unread", format = NA_character_))
      } else for (v in names(parsed)) {
        rows[[length(rows) + 1L]] <- c(base, list(var = v, kind = parsed[[v]]$kind, format = parsed[[v]]$format))
      }
    }
  }
  if (!length(rows)) return(tibble::tibble(unit_id = integer(), line = integer(), file = character(),
    text = character(), var = character(), kind = character(), format = character()))
  tibble::tibble(
    unit_id = vapply(rows, function(r) as.integer(r$unit_id), integer(1)),
    line = vapply(rows, `[[`, integer(1), "line"),
    file = vapply(rows, `[[`, character(1), "file"),
    text = vapply(rows, `[[`, character(1), "text"),
    var = vapply(rows, `[[`, character(1), "var"),
    kind = vapply(rows, `[[`, character(1), "kind"),
    format = vapply(rows, `[[`, character(1), "format")
  )
}

#' Follow one variable's format from the step that writes an output back to
#' its declaration, through steps that are read exactly
#'
#' Supported: a FORMAT or ATTRIB statement in a DATA step; a DATA step whose
#' first SET or MERGE dataset carries the format; PROC SORT. Any macro
#' reference in a step on the way, any step or macro call in between that
#' could change the dataset, a RENAME touching the variable, a DROP or KEEP
#' list that could remove it and is not read exactly, or a dataset this file
#' does not see being created ends the trace as not followed.
#'
#' A DROP or KEEP that removes the variable ends the trace too. It is a proven
#' exclusion, `excluded = TRUE`, only when every step between it and the output
#' reads one dataset: with `merge a(drop=x) b` the variable and its format can
#' still arrive from `b`, so that is reported as not followed.
#'
#' The steps that run after the output is written are read as well, because the
#' output is what they leave. In the same file, macro-generated code or a step
#' that can change the output ends the trace. In another program, a step read
#' exactly that names the output and can change it ends the trace; what
#' macro-generated code there does is not known and is not held against the
#' output. Reading the output, sorting it in place with no option, deleting
#' other members and clearing another library change nothing.
#'
#' @param target The dataset the writer is followed for, as `lib.member`.
#' @return `list(ok = TRUE, format, line, file, text)`, or
#'   `list(ok = FALSE, reason)`, with `excluded = TRUE` when the source proves
#'   that no format declared before the exclusion reaches the output.
#' @noRd
trace_declared_date <- function(trace, writer, var, target) {
  fail <- function(...) list(ok = FALSE, reason = paste0(...))
  at <- function(us) paste0("line ", us$line_start[1L])
  generated <- function(us) {
    any(has_macro_trigger(us$text)) ||
      any(grepl("\\b(call\\s+execute|dosubl)\\b", us$text, ignore.case = TRUE, perl = TRUE))
  }
  member_key <- function(name) {
    name <- tolower(name)
    ifelse(grepl(".", name, fixed = TRUE), name, paste0("work.", name))
  }
  # What a DROP or KEEP that removes the variable means for the trace.
  removed <- function(gone, single, from = NULL,
                      so = "before the output, so the declared date format does not reach it") {
    what <- paste0("the ", gone$op, " list at line ", gone$line, " removes the variable",
                   if (is.null(from)) "" else paste0(" from ", from))
    if (single) return(list(ok = FALSE, excluded = TRUE, reason = paste0(what, " ", so)))
    fail(what, ", and the variable may still come from another dataset read on the way to the output, which is not followed")
  }
  unread <- function(gone) fail("the DROP or KEEP list at line ", gone$line,
                                " could remove the variable and is not read exactly")
  # Where a PROC SORT statement names the dataset after DATA= or OUT=.
  sort_names <- function(lex, key) {
    i <- which(top_level_tokens(lex) & tolower(lex) == key & c(lex[-1L], "") == "=")
    if (length(i) == 1L && i + 2L <= length(lex)) i + 2L else NA_integer_
  }
  name_pattern <- function(name) {
    paste0("(?<![A-Za-z0-9_])", gsub("([^A-Za-z0-9_])", "\\\\\\1", name), "(?![A-Za-z0-9_])")
  }
  names_any <- function(text, pattern) any(grepl(pattern, text, ignore.case = TRUE, perl = TRUE))
  # What a step that does not create `dataset` can still do to it. `NULL` when
  # it does nothing to the variable. `where` says which step it is: one that
  # runs "between" the step that creates the dataset and the step that reads
  # it, one "after" the step that writes the output in the same file, or one
  # "elsewhere", in another program.
  #
  # Between two steps anything that could change a dataset ends the trace. For
  # the finished output the step must name what it changes: clearing WORK at
  # the end of a program, or reading the output in a later one, changes nothing.
  step_effect <- function(us, type, reads, dataset, where, single = TRUE) {
    here <- paste0(at(us), if (where == "elsewhere") paste0(" of ", basename(us$file[1L])) else "")
    when <- switch(where, between = "runs between the declaration and the output",
                   "runs after the step that writes the output")
    parts <- strsplit(dataset, ".", fixed = TRUE)[[1L]]
    delayedAssign("masked", vapply(us$text, mask_strings, "", USE.NAMES = FALSE))
    escaped <- function(x) gsub("([^A-Za-z0-9_])", "\\\\\\1", x)
    written <- if (identical(parts[1L], "work")) paste0("(?:work\\.)?", escaped(parts[2L]), "(?![A-Za-z0-9_.])")
               else paste0(escaped(dataset), "(?![A-Za-z0-9_])")
    # ODS OUTPUT and options such as DUPOUT= write a dataset the lineage does
    # not record.
    unrecorded <- function(rows = TRUE) {
      if (where == "between" || dataset %in% reads || !names_any(masked[rows], paste0("=\\s*", written))) return(NULL)
      fail("a statement at ", here, " may write ", dataset, " in a way that is not followed")
    }
    if (identical(type, "global")) {
      if (where == "elsewhere") return(if (any(has_macro_trigger(us$text))) NULL else unrecorded(us$first_token == "ods"))
      text <- sub("^\\s*%(let|put|global|local|symdel)\\b", "", us$text, ignore.case = TRUE, perl = TRUE)
      plain <- us$first_token %in% c("%let", "%put", "%global", "%local", "%symdel")
      if (any(grepl("%", vapply(text[plain], mask_strings, "", keep_double = TRUE, USE.NAMES = FALSE), fixed = TRUE)) ||
          any(has_macro_trigger(text[!plain])))
        return(fail("macro-generated code at ", here, " ", when))
      if (where == "between" && any(us$first_token == "libname"))
        return(fail("a LIBNAME statement at ", here, " ", when))
      return(unrecorded(us$first_token == "ods"))
    }
    # What macro-generated code in another program does is not known from the
    # source, and holding every earlier output to it would leave nothing typed.
    if (generated(us)) return(if (where == "elsewhere") NULL else fail("macro-generated code at ", here, " ", when))
    proc <- if (identical(type, "proc_step")) tolower(sub("^\\s*proc\\s+([A-Za-z0-9_]+).*$", "\\1", us$text[1L], ignore.case = TRUE)) else ""
    if (where == "between") {
      if (proc %in% DATE_TRACE_MODIFYING_PROCS || any(us$first_token == "modify"))
        return(fail("a step at ", here, " can change datasets in place"))
    } else {
      member <- names_any(us$text, name_pattern(parts[2L]))
      library <- name_pattern(parts[1L])
      # A copy replaces the members of the library it writes to: the one after
      # OUT=, or the procedure's own library for a COPY statement without it.
      copies_into <- function(rows) {
        if (names_any(masked[rows], paste0("\\bout(?:lib)?\\s*=\\s*", library))) return(TRUE)
        own <- regmatches(masked[1L], regexec("\\b(?:library|lib|ddname|dd)\\s*=\\s*([A-Za-z_][A-Za-z0-9_]*)",
                                              masked[1L], ignore.case = TRUE, perl = TRUE))[[1L]]
        own <- if (length(own) == 2L) tolower(own[2L]) else "work"
        identical(proc, "datasets") && identical(own, parts[1L]) &&
          !all(grepl("\\bout(?:lib)?\\s*=", masked[rows], ignore.case = TRUE, perl = TRUE))
      }
      changes <- switch(proc,
        # DELETE, SAVE and CONTENTS leave the variables of what remains alone.
        datasets = (member && !all(us$first_token %in% c("proc", "delete", "save", "contents", "run", "quit"))) ||
          (any(us$first_token == "copy") && copies_into(us$first_token == "copy")),
        # Only ALTER TABLE changes the columns of a table that exists.
        sql = member && any(us$first_token == "alter"),
        copy = copies_into(1L),
        cimport = member || names_any(us$text, library),
        fedsql = , ds2 = , iml = member,
        FALSE)
      if (changes) return(fail("a step at ", here, " can change ", dataset, " in place"))
    }
    # PROC SORT with no OUT= rewrites its input with the options on DATA=.
    # The lineage records that as a read, so it is met here and not as a
    # writer.
    if (identical(proc, "sort")) {
      lex <- dataset_tokens(us$text[1L])$lex
      data_at <- sort_names(lex, "data")
      if (!is.na(data_at) && is.na(sort_names(lex, "out")) && identical(member_key(lex[data_at]), dataset)) {
        if (grepl("\\brename\\s*=", mask_strings(us$text[1L]), ignore.case = TRUE, perl = TRUE))
          return(fail("a RENAME= option at ", here, " may rename the variable"))
        if (!all(us$first_token[-1L] %in% c("by", "run", "quit")))
          return(fail("the PROC SORT at ", here, " rewrites ", dataset, " with a statement that is not followed"))
        gone <- projection_excludes(dataset_option_filters(lex, data_at, us$line_start[1L]), var)
        if (is.na(gone$excluded)) return(unread(gone))
        if (isTRUE(gone$excluded)) return(switch(where,
          between = removed(gone, single, from = dataset),
          after = removed(gone, TRUE, from = dataset, so = "after it is written, so the declared date format is not in the output"),
          fail("the ", gone$op, " list at ", here, " may remove the variable from ", dataset, " after it is written")))
        return(NULL)
      }
    }
    # A hash object writes the dataset its OUTPUT method names in a string.
    if (identical(type, "data_step")) {
      if (where != "between" && names_any(us$text, paste0("['\"]\\s*", written)))
        return(fail("a statement at ", here, " may write ", dataset, " in a way that is not followed"))
      return(NULL)
    }
    unrecorded()
  }
  # The latest step that does something decides: a step that cannot be read
  # hides what came before it, and a removal holds whatever came before it.
  latest_effect <- function(ids, dataset, where, single = TRUE) {
    for (mid in rev(ids)) {
      found <- step_effect(trace$statements(mid), trace$type(mid), trace$reads(mid), dataset, where, single)
      if (!is.null(found)) return(found)
    }
    NULL
  }
  follow_input <- function(id, dataset, depth, single) {
    prior <- trace$prior_writer(id, dataset)
    if (is.na(prior)) return(fail(dataset, " is not created earlier in this program, so its formats are not known from the source"))
    found <- latest_effect(trace$between(prior, id), dataset, "between", single)
    if (!is.null(found)) return(found)
    follow(prior, dataset, depth + 1L, single)
  }
  # `member` is the dataset this step is followed for. `single` says every step
  # between this one and the output reads one dataset.
  follow <- function(id, member, depth, single) {
    if (depth > 50L) return(fail("the steps before the output form too long a chain to follow"))
    us <- trace$statements(id)
    type <- trace$type(id)
    if (generated(us)) return(fail("the step at ", at(us), " contains macro-generated code"))
    lex <- dataset_tokens(us$text[1L])$lex
    top <- top_level_tokens(lex)
    if (identical(type, "proc_step")) {
      if (!grepl("^\\s*proc\\s+sort\\b", us$text[1L], ignore.case = TRUE, perl = TRUE))
        return(fail("the procedure at ", at(us), " is not one whose output formats are followed"))
      if (!all(us$first_token[-1L] %in% c("by", "run", "quit")) ||
          grepl("\\brename\\s*=", mask_strings(us$text[1L]), ignore.case = TRUE, perl = TRUE))
        return(fail("the PROC SORT at ", at(us), " has a statement or option that is not followed"))
      input <- trace$reads(id)
      if (length(input) != 1L) return(fail("the PROC SORT at ", at(us), " does not read exactly one dataset"))
      data_at <- sort_names(lex, "data")
      out_at <- sort_names(lex, "out")
      written <- if (is.na(out_at)) input else member_key(lex[out_at])
      if (!identical(written, member))
        return(fail("the PROC SORT at ", at(us), " does not write ", member, " with OUT= or in place"))
      # PROC SORT creates no variable: one removed on the way in or on the way
      # out is not in the dataset it writes.
      gone <- projection_excludes(c(
        if (!is.na(data_at)) dataset_option_filters(lex, data_at, us$line_start[1L]),
        if (!is.na(out_at)) dataset_option_filters(lex, out_at, us$line_start[1L])), var)
      if (isTRUE(gone$excluded)) return(removed(gone, single))
      if (is.na(gone$excluded)) return(unread(gone))
      return(follow_input(id, input, depth, single))
    }
    if (!identical(type, "data_step")) return(fail("the step at ", at(us), " is not a DATA step or PROC SORT"))
    if (grepl("\\brename\\s*=|/", mask_strings(us$text[1L]), ignore.case = TRUE, perl = TRUE))
      return(fail("the DATA statement at ", at(us), " has an option that is not followed"))
    # What the step writes to `member`: a DROP or KEEP statement applies to
    # every dataset the step writes, a dataset option to that dataset only.
    # This comes before the step's own FORMAT statement, because a variable the
    # step leaves out takes that format with it.
    member_at <- which(top & seq_along(lex) > 1L & member_key(lex) == member)
    if (length(member_at) != 1L)
      return(fail("the DATA statement at ", at(us), " does not name ", member, " exactly once"))
    lists <- which(us$first_token %in% c("drop", "keep") &
                     !grepl("^\\s*(drop|keep)\\s*=", us$text, ignore.case = TRUE, perl = TRUE))
    gone <- projection_excludes(c(
      lapply(lists, function(k) read_projection_list(us$first_token[k],
        sub("^\\s*(drop|keep)\\b", "", us$text[k], ignore.case = TRUE, perl = TRUE), us$line_start[k])),
      dataset_option_filters(lex, member_at, us$line_start[1L])), var)
    if (isTRUE(gone$excluded)) return(removed(gone, single))
    if (is.na(gone$excluded)) return(unread(gone))
    state <- NULL
    for (k in seq_len(nrow(us))) {
      token <- us$first_token[k]
      if (token %in% c("format", "attrib")) {
        parsed <- parse_format_declaration(us$text[k])
        if (is.null(parsed)) state <- list(kind = "unread", line = us$line_start[k])
        else if (!is.null(parsed[[var]])) state <- c(parsed[[var]], list(line = us$line_start[k],
          file = us$file[k], text = trimws(gsub("\\s+", " ", us$text[k]))))
      } else if (token == "rename") {
        pairs <- parse_rename_statement(us$text[k])
        if (is.null(pairs) || var %in% pairs) return(fail("a RENAME statement at line ", us$line_start[k], " may rename the variable"))
      }
    }
    if (!is.null(state)) {
      if (state$kind == "date") return(list(ok = TRUE, format = state$format, line = as.integer(state$line),
        file = state$file, text = state$text))
      if (state$kind == "unread") return(fail("a FORMAT or ATTRIB statement at line ", state$line, " is not read exactly"))
      return(fail("the step at ", at(us), " leaves the variable without a date format"))
    }
    readers <- which(us$first_token %in% c("set", "merge", "update", "modify"))
    if (length(readers) != 1L || !us$first_token[readers] %in% c("set", "merge"))
      return(fail("the step at ", at(us), " does not read its data with one SET or MERGE statement"))
    # `if 0 then set lookup;` reads a second dataset that no statement starts
    # with, and its formats come first when it is written first.
    if (any(grepl("\\b(then|else)\\s+(set|merge|update|modify)\\b",
                  vapply(us$text, mask_strings, "", USE.NAMES = FALSE), ignore.case = TRUE, perl = TRUE)))
      return(fail("the step at ", at(us), " reads a dataset inside an IF statement, which is not followed"))
    first <- regmatches(us$text[readers], regexec(
      "^\\s*(?:set|merge)\\s+([A-Za-z_][A-Za-z0-9_]*(?:\\.[A-Za-z_][A-Za-z0-9_]*)?)\\s*(\\([^()]*(?:\\([^()]*\\)[^()]*)*\\))?",
      us$text[readers], ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(first) < 2L) return(fail("the ", toupper(us$first_token[readers]), " statement at line ", us$line_start[readers], " is not read exactly"))
    if (length(first) >= 3L && grepl("\\brename\\s*=", first[3L], ignore.case = TRUE, perl = TRUE))
      return(fail("a RENAME= option at line ", us$line_start[readers], " may rename the variable"))
    dataset <- member_key(first[2L])
    # The format is followed through the first dataset only. Where that is the
    # step's one input, a variable removed on the way in is new in this step.
    one <- identical(trace$reads(id), dataset)
    gone <- projection_excludes(dataset_option_filters(
      dataset_tokens(us$text[readers])$lex, 2L, us$line_start[readers]), var)
    if (isTRUE(gone$excluded)) return(removed(gone, single && one, from = dataset))
    if (is.na(gone$excluded)) return(unread(gone))
    follow_input(id, dataset, depth, single && one)
  }
  # The output is what is left when the programs have run, so the steps that
  # run after its writer are read first. Another program's place in the run is
  # not known here, so a step there that can change the output always counts.
  output <- tolower(target)
  parts <- strsplit(output, ".", fixed = TRUE)[[1L]]
  for (step in trace$outside(name_pattern(parts[2L]), name_pattern(parts[1L]))) {
    found <- step_effect(step$us, step$type, step$reads, output, "elsewhere")
    if (!is.null(found)) return(found)
  }
  found <- latest_effect(trace$after(writer), output, "after")
  if (!is.null(found)) return(found)
  follow(writer, output, 0L, TRUE)
}

#' The kind of each column in a reference dataset
#'
#' Reads no rows from a SAS file: the column types are in its header.
#'
#' @return A named character vector, lower-case column name to
#'   [col_kind()], or `NULL` when the file is absent or unreadable.
#' @noRd
reference_column_kinds <- function(path) {
  if (!is_scalar_character(path) || is.na(path) || !nzchar(path) || !file.exists(path) || dir.exists(path)) return(NULL)
  data <- tryCatch(switch(tolower(tools::file_ext(path)),
    rds = readRDS(path),
    xpt = haven::read_xpt(path, n_max = 0L),
    sas7bdat = haven::read_sas(path, n_max = 0L),
    NULL), error = function(e) NULL)
  if (!inherits(data, "data.frame")) return(NULL)
  stats::setNames(vapply(data, col_kind, character(1)), tolower(names(data)))
}

empty_date_columns <- function() {
  tibble::tibble(var = character(), format = character(), evidence = character(),
                 file = character(), line = integer(), declaration = character(),
                 reason = character())
}

#' Decide which output columns are SAS dates
#'
#' @param project A `sas2r_project`.
#' @param diagnostics Also list reference date columns that have no
#'   declaration. Off when only the bundle's own list is wanted.
#' @return A named list keyed by output dataset. Each entry holds two tibbles
#'   with the columns of [empty_date_columns()]: `typed`, the columns the
#'   bundle writes as `Date` (`evidence` is `"reference"` or `"source"`), and
#'   `deferred`, the columns left as SAS numbers with the `reason`.
#' @noRd
source_date_outputs <- function(project, diagnostics = TRUE) {
  contracts <- project$output_contracts
  lineage <- project$lineage
  statements <- project$statements
  if (is.null(contracts) || !nrow(contracts) || is.null(lineage) || is.null(statements)) return(list())
  contracts <- contracts[contracts$kind == "dataset" & !is.na(contracts$logical_name), , drop = FALSE]
  if (!nrow(contracts)) return(list())
  rules <- project$config$outputs %||% list()
  writer_of <- function(target) {
    ids <- unique(lineage$unit_id[lineage$role == "creates" & lineage$dataset == target])
    ids <- ids[ids %in% statements$unit_id[statements$unit_type != "macro_def"]]
    files <- unique(statements$file[statements$unit_id %in% ids])
    if (!length(ids) || length(files) != 1L) return(NULL)
    trace <- date_trace_file(project, files)
    list(trace = trace, id = trace$ids[max(match(ids, trace$ids))])
  }
  targets <- tolower(contracts$logical_name)
  writers <- stats::setNames(lapply(targets, writer_of), targets)
  position <- vapply(writers, function(w) if (is.null(w)) NA_integer_ else
    match(w$id, project$order %||% unique(statements$unit_id)), integer(1))
  out <- list()
  for (target in targets[order(position, na.last = TRUE)]) {
    w <- writers[[target]]
    contract <- as.list(contracts[match(target, targets), , drop = FALSE])
    typed <- deferred <- empty_date_columns()
    add <- function(to, var, format, evidence, at, reason = NA_character_) {
      rbind(to, tibble::tibble(var = var, format = format %||% NA_character_, evidence = evidence,
        file = at$file %||% NA_character_, line = as.integer(at$line %||% NA_integer_),
        declaration = at$text %||% NA_character_, reason = reason))
    }
    candidates <- list()
    kinds <- NULL
    if (!is.null(w)) {
      ancestors <- date_trace_ancestors(w$trace, w$id)
      declared <- date_declarations(w$trace, ancestors)
      upstream <- intersect(unlist(lapply(ancestors, w$trace$reads)), setdiff(names(out), target))
      upstream <- upstream[vapply(upstream, function(source) nrow(out[[source]]$typed) > 0L, logical(1))]
      # The reference is read only when there is something for it to confirm.
      if (any(declared$kind == "date") || length(upstream) || isTRUE(diagnostics))
        kinds <- reference_column_kinds(output_reference_path(contract, rules))
      for (var in unique(declared$var[declared$kind == "date"])) {
        rows <- declared[!is.na(declared$var) & declared$var == var, , drop = FALSE]
        last <- rows[max(which(rows$kind == "date")), , drop = FALSE]
        candidates[[var]] <- list(format = last$format, file = last$file, line = last$line, text = last$text,
          contradicted = any(rows$kind != "date"), inherited = NA_character_)
      }
      # A format declared for an upstream output travels with that dataset.
      # It is considered only where the reference has the column as a date:
      # nothing in the source says the column is kept in this output at all.
      for (source in upstream) {
        up <- out[[source]]$typed
        for (k in seq_len(nrow(up))) {
          var <- up$var[k]
          if (!is.null(candidates[[var]]) || !identical(unname(kinds[var]), "date")) next
          own <- declared[!is.na(declared$var) & declared$var == var, , drop = FALSE]
          candidates[[var]] <- list(format = up$format[k], file = up$file[k], line = up$line[k],
            text = up$declaration[k], contradicted = nrow(own) > 0L, inherited = source)
        }
      }
    } else if (isTRUE(diagnostics)) {
      kinds <- reference_column_kinds(output_reference_path(contract, rules))
    }
    for (var in names(candidates)) {
      cand <- candidates[[var]]
      kind <- if (var %in% names(kinds)) unname(kinds[[var]]) else NA_character_
      traced <- trace_declared_date(w$trace, w$id, var, target)
      if (isTRUE(traced$excluded)) {
        # The source proves the declared format stops before this output, so
        # the column there is not that date and the reference cannot make it
        # one. It is listed only where the reference stores a date, which the
        # source then does not explain.
        if (identical(kind, "date")) deferred <- add(deferred, var, cand$format, "source", cand, paste0(
          traced$reason, "; the reference stores a date, which the SAS source read here does not explain"))
        next
      }
      if (!is.na(cand$inherited))
        traced <- list(ok = FALSE, reason = paste0("the format is declared for ", cand$inherited, ", which this program reads"))
      if (!is.na(kind) && kind != "date") {
        deferred <- add(deferred, var, cand$format, "source", cand,
          paste0("the SAS source declares a date format, but the reference stores this column as ", kind))
      } else if (!is.na(kind) && (isTRUE(traced$ok) || !cand$contradicted)) {
        typed <- add(typed, var, if (isTRUE(traced$ok)) traced$format else cand$format, "reference",
          if (isTRUE(traced$ok)) traced else cand)
      } else if (is.na(kind) && isTRUE(traced$ok)) {
        typed <- add(typed, var, traced$format, "source", traced)
      } else {
        deferred <- add(deferred, var, cand$format, "source", cand, paste0(
          if (cand$contradicted && !is.na(kind)) "another statement on the way to the output gives this variable a different format; "
          else "", traced$reason,
          if (is.na(kind)) "; there is no reference column to confirm the date format" else ""))
      }
    }
    if (isTRUE(diagnostics)) {
      for (var in setdiff(names(kinds)[kinds == "date"], names(candidates))) {
        deferred <- add(deferred, var, NA_character_, "reference", list(),
          "the reference stores a date, but no date format declaration for this variable was read from the SAS source")
      }
    }
    out[[target]] <- list(typed = typed, deferred = deferred)
  }
  out[targets]
}

#' The date columns a bundle writes as `Date`, for `_sas2r_formats.R`
#'
#' A failure here must not stop a run: the columns stay SAS numbers, exactly as
#' they were before this file existed, and the comparison reports the type.
#'
#' @return A named list, output dataset to a named character vector of
#'   column name and SAS format. Empty when nothing is declared.
#' @noRd
output_date_registry <- function(project) {
  decided <- tryCatch(source_date_outputs(project, diagnostics = FALSE), error = function(e) list())
  registry <- lapply(decided, function(x) stats::setNames(as.character(x$typed$format), x$typed$var))
  registry <- registry[vapply(registry, length, integer(1)) > 0L]
  if (length(registry)) registry else list()
}

#' One output's date columns as plain records, for the run report
#'
#' @param decided One entry of [source_date_outputs()].
#' @return `NULL` when nothing is declared or deferred, otherwise
#'   `list(typed, deferred)`, each a list of records.
#' @noRd
date_columns_record <- function(decided) {
  if (is.null(decided) || (!nrow(decided$typed) && !nrow(decided$deferred))) return(NULL)
  records <- function(df) lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    row$file <- if (is.na(row$file)) NA_character_ else basename(row$file)
    row[!vapply(row, function(v) is.na(v), logical(1))]
  })
  list(typed = records(decided$typed), deferred = records(decided$deferred))
}

#' Report lines for the date columns of every output
#'
#' @param output_assessments Named list of output assessments.
#' @return Markdown lines, empty when no output has a declared or deferred
#'   date column.
#' @noRd
date_columns_report_lines <- function(output_assessments) {
  lines <- character()
  for (key in names(output_assessments)) {
    cols <- output_assessments[[key]]$date_columns
    for (x in cols$typed) lines <- c(lines, paste0(
      "- **", key, "** `", x$var, "`: stored as a date (SAS format ", x$format,
      if (!is.null(x$file)) paste0(", declared at ", x$file, ":", x$line) else "",
      if (identical(x$evidence, "reference")) "; the reference stores a date" else
        "; followed from the declaration to the output", ")."))
    for (x in cols$deferred) lines <- c(lines, paste0(
      "- **", key, "** `", x$var, "`: left as a SAS day number. ",
      toupper(substr(x$reason, 1L, 1L)), substring(x$reason, 2L), "."))
  }
  if (!length(lines)) return(character())
  c("## Date Columns", "",
    paste("SAS stores a date as a number of days from 1960-01-01 and a format that shows it as a date.",
          "A column the SAS source declares with a date format is stored as an R `Date`;",
          "the generated programs still compute with the SAS day number."),
    "", lines, "")
}
