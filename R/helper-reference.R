# Build prompt documentation from the same Rd pages generated from runtime
# roxygen. Signatures always come from the live runtime functions.
runtime_helper_documentation <- function(man_dir) {
  result <- list()
  flatten <- function(x) paste(unlist(x, use.names = FALSE), collapse = "")
  for (file in list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)) {
    rd <- tools::parse_Rd(file)
    tags <- vapply(rd, function(x) attr(x, "Rd_tag") %||% "", character(1))
    aliases <- vapply(rd[tags == "\\alias"], flatten, character(1))
    helpers <- intersect(aliases, SAS2R_HELPER_NAMES)
    if (!length(helpers)) next
    section_text <- function(tag) trimws(paste(vapply(rd[tags == tag], flatten, character(1)), collapse = "\n"))
    sections <- c(section_text("\\description"), section_text("\\details"))
    args <- rd[tags == "\\arguments"]
    argument_text <- character()
    dots <- FALSE
    if (length(args)) for (item in args[[1]]) {
      if (!identical(attr(item, "Rd_tag"), "\\item")) next
      name <- flatten(item[[1]])
      text <- trimws(flatten(item[[2]]))
      argument_text <- c(argument_text, paste0(name, ": ", text))
      if (identical(name, "...")) dots <- !grepl("Ignored|Not used", text)
    }
    if (length(argument_text)) sections <- c(sections, paste(c("Arguments:", argument_text), collapse = "\n"))
    value <- section_text("\\value")
    if (nzchar(value)) sections <- c(sections, paste("Returns:", value, sep = "\n"))
    examples <- section_text("\\examples")
    if (nzchar(examples)) sections <- c(sections, paste("Examples:", examples, sep = "\n"))
    description <- paste(sections[nzchar(sections)], collapse = "\n\n")
    for (helper in helpers) result[[helper]] <- list(text = description, dots = dots)
  }
  result[sort(names(result), method = "radix")]
}

helper_documentation <- function() {
  path <- system.file("templates", "helper-reference.json", package = "sas2r")
  if (!nzchar(path) || !file.exists(path)) {
    cli::cli_abort(c("Required runtime helper reference is missing.",
      "i" = "Reinstall sas2r, or use {.code pkgload::load_all()} from the current source checkout."),
      class = "sas2r_helper_reference_missing")
  }
  jsonlite::read_json(path, simplifyVector = FALSE)
}

helper_call_definition <- function(name, docs = helper_documentation()) {
  fn <- get(name, envir = asNamespace("sas2r"), inherits = FALSE)
  # Some helpers retain ... for informative runtime errors or ignore it.
  # It never authorizes invented behavior such as join= or all.x=.
  if ("..." %in% names(formals(fn)) && !isTRUE(docs[[name]]$dots)) {
    formals(fn) <- formals(fn)[names(formals(fn)) != "..."]
  }
  fn
}

helper_reference <- function() {
  docs <- helper_documentation()
  seen <- character()
  entries <- vapply(SAS2R_HELPER_NAMES, function(name) {
    fn <- helper_call_definition(name, docs)
    signature <- paste(deparse(args(fn), width.cutoff = 120L), collapse = " ")
    signature <- sub("^function ", name, sub(" NULL$", "", signature))
    description <- docs[[name]]$text %||% ""
    if (description %in% seen) description <- ""
    else seen <<- c(seen, description)
    paste(signature, description, sep = "\n")
  }, character(1))
  paste("Authoritative sas2r runtime interfaces (available without tool calls).",
        "Use these exact call forms. Unsupported semantics must be deferred, not approximated.",
        paste(entries, collapse = "\n\n"), sep = "\n")
}

helper_call_misuse <- function(call, name, docs) {
  # Let R's own argument matching handle named and positional calls, without
  # evaluating any generated expressions. Dynamic ... forwarding is deferred
  # to execution; direct invented arguments are rejected here.
  fn <- helper_call_definition(name, docs)
  tryCatch({
    if (any(vapply(as.list(call)[-1], identical, logical(1), quote(...)))) return(NULL)
    match.call(definition = fn, call = call, expand.dots = FALSE)
    NULL
  }, error = function(e) paste0(name, "(): ", conditionMessage(e)))
}

# Contract metadata is derived from code and resolved project interfaces. A
# model's helper_use must never turn a project macro into a runtime helper.
r_call_names <- function(code) {
  exprs <- tryCatch(parse(text = code), error = function(e) NULL)
  calls <- character()
  walk <- function(e) {
    if (is.call(e)) {
      head <- e[[1L]]
      if (is.name(head)) calls <<- c(calls, as.character(head))
      else if (is.call(head) && identical(head[[1L]], as.name("::")) &&
               identical(head[[2L]], as.name("sas2r"))) {
        calls <<- c(calls, as.character(head[[3L]]))
      }
    }
    if (is.call(e) || is.expression(e) || is.pairlist(e)) {
      for (i in seq_along(e)) {
        if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
      }
    }
  }
  walk(exprs)
  unique(calls)
}

reconcile_helper_use <- function(code, declared = character(),
                                 dependency_functions = character(), refresh = FALSE) {
  calls <- r_call_names(code)
  unknown <- setdiff(unlist(declared), c(SAS2R_HELPER_NAMES, dependency_functions))
  # Repairs cannot retain claims about helpers no longer called by their code.
  # Still-used unknown helpers remain errors, rather than being authorized.
  if (isTRUE(refresh)) unknown <- intersect(unknown, calls)
  unique(c(intersect(calls, SAS2R_HELPER_NAMES), unknown))
}
