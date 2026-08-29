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

#' Test whether a comma-separated flag string carries a flag
#'
#' The read side of [add_flag()]. Manifest `flags` is one comma-separated
#' string per row, so a flag cannot be tested with `==` -- a row that gained a
#' second flag would stop matching.
#'
#' @param flags Character vector of comma-separated flag strings.
#' @param flag One flag name.
#' @return A logical vector as long as `flags`.
#' @noRd
has_flag <- function(flags, flag) {
  vapply(flags, function(f) {
    if (length(f) != 1L || is.na(f) || !nzchar(trimws(f))) return(FALSE)
    flag %in% trimws(strsplit(f, ",", fixed = TRUE)[[1]])
  }, logical(1), USE.NAMES = FALSE)
}

#' Test whether one manifest row carries a flag
#'
#' Manifest-shaped wrapper over [has_flag()], defaulting to `FALSE` for a
#' caller that supplied no manifest so a missing manifest can never be read as
#' a positive claim about a unit.
#'
#' @param manifest A translation manifest, or `NULL`.
#' @param unit_id One unit id.
#' @param flag One flag name.
#' @return `TRUE` when a row for `unit_id` carries `flag`.
#' @noRd
unit_carries_flag <- function(manifest, unit_id, flag) {
  if (is.null(manifest) || !is.data.frame(manifest) ||
      !all(c("unit_id", "flags") %in% names(manifest))) {
    return(FALSE)
  }
  hit <- !is.na(manifest$unit_id) & manifest$unit_id == unit_id
  if (!any(hit)) return(FALSE)
  any(has_flag(as.character(manifest$flags[hit]), flag))
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



