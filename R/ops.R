#' @include functional.R
NULL

#' A null coalescing operator.
#' 
#' If the LHS eveluates to \code{NULL}, a zero-length vector, or
#' an empty string, return the RHS, otherwise return the LHS.
#' 
#' @param a Some value.
#' @param b Some default value.
#' @return \code{a}, if \code{a} is not \code{NULL}, not an empty
#' vector, not an empty string, or not \code{NA}.
#' @rdname default_ops
#' @export
#' @examples
#' a1 <- "some value"
#' a2 <- ""
#' b <- "default value"
#' a1 %||% b
#' a2 %||% b
#' @keywords utilities 
"%||%" <- function (a, b, filter = "is.empty") {
  filter <- match.fun(filter)
  if (filter(a)) b else a
}

#' @details \code{\%|\%} is a vectorized version of \code{\%||\%}
#' @rdname default_ops 
#' @export
"%|%" <- function (a, b, filter = "are_empty") {
  filter <- match.fun(filter)
  ifelse(filter(a), b, a)
}

#' @usage a \%|na|\% b
#' @details \code{\%|na|\%} returns the RHS if the LHS evaluates to \code{NA}
#' @rdname default_ops 
#' @export
"%|na|%" <- Partial(`%||%`, filter = "is.na")

#' @usage a \%|NA|\% b
#' @details \code{\%|NA|\%} is a vectorized version of \code{\%|na|\%}
#' @rdname default_ops 
#' @export
"%|NA|%" <- Partial(`%|%`, filter = "is.na")

#' @usage a \%|null|\% b
#' @details \code{\%|null|\%} explicitly works only on \code{NULL} but
#' not empty strings and vectors.
#' @rdname default_ops 
#' @export
"%|null|%" <- Partial(`%||%`, filter = "is.null")

#' Reverse Value Matching
#' 
#' Negation of the \code{\link{\%in\%}} operator.
#' 
#' @usage x \%ni\% y
#' @param x The values to be matched.
#' @param y The values to \emph{not} be matched against.
#' @return A logical vector.
#' @rdname ni
#' @export
#' @examples
#' 1:5 %ni% c(3,4,5)
#' @keywords utilities
"%ni%" <- Negate(`%in%`)

#' A simple pattern matching operator.
#' 
#' @param x A character vector.
#' @param pattern A regular expression.
#' @return Matching elements in \code{target}
#' @export
"%~%" <- function(x, pattern) grep(pattern, x, value = TRUE)

