ordered_fixture <- function(code, order = names(code), envir = parent.frame(), libraries = list()) {
  root <- withr::local_tempdir(.local_envir = envir)
  for (name in names(code)) {
    file <- file.path(root, name)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(code[[name]], file)
  }
  config <- list(migration = list(execution_order = order), libraries = libraries)
  list(root = root, config = config)
}
