#' Add flags to comma-separated flag strings
#'
#' @param flags Character vector of existing comma-separated flags.
#' @param new Character vector of flag names to add.
#' @return Character vector of updated flag strings.
#' @noRd
add_flag <- function(flags, new) {
  if (length(flags) == 0L) return(character(0))
  new_clean <- unique(new[!is.na(new) & nzchar(trimws(new))])
  if (length(new_clean) == 0L) return(flags)
  vapply(flags, function(f) {
    existing <- if (is.na(f) || !nzchar(trimws(f))) character(0)
                else trimws(strsplit(f, ",")[[1]])
    existing <- existing[nzchar(existing)]
    paste(unique(c(existing, new_clean)), collapse = ",")
  }, character(1), USE.NAMES = FALSE)
}

#' @noRd
is_scalar_character <- function(x, allow_empty = FALSE) {
  is.character(x) && length(x) == 1L && !is.na(x) && (allow_empty || nzchar(x))
}

#' @noRd
is_positive_integer <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  length(y) == 1L && !is.na(y) && is.finite(y) && y >= 1 && y == floor(y)
}

#' @noRd
path_is_absolute_or_escaping <- function(path) {
  if (!is_scalar_character(path)) return(TRUE)
  normalized <- gsub("\\\\", "/", path)
  if (grepl("^(?:/|[A-Za-z]:/|//)", normalized, perl = TRUE)) return(TRUE)
  components <- strsplit(normalized, "/", fixed = TRUE)[[1]]
  !length(components) || any(!nzchar(components)) ||
    any(components %in% c(".", ".."))
}
