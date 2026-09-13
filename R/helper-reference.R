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
    description <- trimws(paste(vapply(rd[tags %in% c("\\description", "\\details", "\\examples")], flatten, character(1)), collapse = "\n"))
    args <- rd[tags == "\\arguments"]
    dots <- FALSE
    if (length(args)) for (item in args[[1]]) {
      if (identical(attr(item, "Rd_tag"), "\\item") && identical(flatten(item[[1]]), "...")) {
        dots <- !grepl("Ignored|Not used", flatten(item[[2]]))
      }
    }
    for (helper in helpers) result[[helper]] <- list(text = description, dots = dots)
  }
  result[sort(names(result), method = "radix")]
}

helper_documentation <- function() {
  jsonlite::read_json(system.file("templates", "helper-reference.json", package = "sas2r"), simplifyVector = FALSE)
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
