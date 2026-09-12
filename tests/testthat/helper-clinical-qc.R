review_source <- function(root, code) {
  file <- file.path(root, "main.sas")
  writeLines(code, file)
  file
}
