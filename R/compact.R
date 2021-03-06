#' @include tests.R
NULL

#' Filter "empty" elements from a list.
#' 
#' \code{compact} filters \code{NULL} elments from a list; \code{compactChar} filters
#' empty strings (\code{""}) from a list (or vector); \code{compactNA} filters
#' \code{NA}s.
#' 
#' @param x A vector or list.
#' @return A vector or list with empty elements filtered out.
#' @export
#' @rdname compact
#' @examples
#' l <- list(a=1, b=NULL, c=NA)
#' compact(l)
#' ## $a
#' ## [1] 1
#' ## 
#' ## $c
#' ## [1] NA
#' compactNA(l)
#' ## $a
#' ## [1] 1
#' ## 
#' ## $b
#' ## NULL
compact <- function(x) {
  x[!vapply(x, is.null, FALSE, USE.NAMES = FALSE)]
}

#' @rdname compact
#' @export
compactChar <- function(x) {
  x[vapply(x, nzchar, FALSE, USE.NAMES = FALSE)]
}

#' @rdname compact
#' @export
compactNA <- function(x) {
  filter_na <- function(x) suppressWarnings(is.na(x)) %||% FALSE
  x[!vapply(x, filter_na, FALSE, USE.NAMES = FALSE)]
}
