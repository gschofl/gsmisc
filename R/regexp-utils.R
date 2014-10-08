#' A pattern matching operator.
#'
#' @param x A character vector to match \code{re} against.
#' @param re A regular expression.
#' @return A logical vector of matching elements in \code{x}.
#' @rdname matching-op
#' @family Operators
#' @export
"%~%" <- function(x, re) grepl(re, x, fixed = FALSE)
#' @rdname matching-op
#' @export
"%~f%" <- function(x, re) grepl(re, x, fixed = TRUE)

#' A pattern extraction operator.
#'
#' Extract matching elements from a character vector.
#'
#' @param x A character vector to match \code{re} against.
#' @param re A regular expression.
#' @return The matching elements in \code{x}.
#' @rdname extracting-op
#' @family Operators
#' @export
"%<~%" <- function(x, re) grep(re, x, value = TRUE, fixed = FALSE)
#' @rdname extracting-op
#' @family Operators
#' @export
"%<~f%" <- function(x, re) grep(re, x, value = TRUE, fixed = TRUE)

#' A pattern counting operator.
#'
#' Count how often a pattern occurs in a character vector.
#'
#' @param x A character vector to match \code{re} against.
#' @param re A regular expression.
#' @return The count of the elements in
#' @rdname counting-op
#' @family Operators
#' @export
#' @examples
#' c("foo", "bar", "baz") %n~% "^b"
"%n~%" <- function(x, re) {
  vapply(gregexpr(re, x, fixed = FALSE), function(x) sum(x > 0L), 0, USE.NAMES = FALSE)
}
#' @rdname counting-op
#' @family Operators
#' @export
"%n~f%" <- function(x, re) {
  vapply(gregexpr(re, x, fixed = TRUE), function(x) sum(x > 0L), 0, USE.NAMES = FALSE)
}
