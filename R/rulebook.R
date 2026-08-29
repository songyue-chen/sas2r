.rulebook_env <- new.env(parent = emptyenv())

read_rulebook_yaml <- function(path) {
  yaml::read_yaml(path, eval.expr = FALSE)
}

load_rulebook <- function() {
  if (!is.null(.rulebook_env$rb)) return(.rulebook_env$rb)
  dir <- system.file("rulebook", package = "sas2r")
  rb <- list(
    functions = unlist(read_rulebook_yaml(file.path(dir, "functions.yml"))),
    operators = unlist(read_rulebook_yaml(file.path(dir, "operators.yml"))),
    procs = read_rulebook_yaml(file.path(dir, "procs.yml")),
    semantics = load_semantic_registry()
  )
  .rulebook_env$rb <- rb
  rb
}
