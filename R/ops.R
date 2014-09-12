#' @include functional.R
NULL

#' @export
"%||%" <- function (a, b, filter="is.empty") {
  filter <- match.fun(filter)
  if (filter(a)) b else a
}

## Vectorized version of %||%
#' @export
"%|%" <- function (a, b, filter="are_empty") {
  filter <- match.fun(filter)
  ifelse(filter(a), b, a)
}

#' @export
"%|null|%" <- Partial(`%||%`, filter="is.null")

#' @export
"%|na|%" <- Partial(`%||%`, filter="is.na")

## Chain functions
#' @export
"%@%" <- function(x, f) {
  eval.parent(as.call(append(as.list(substitute(f)), list(x), 1)))
}

## http://stackoverflow.com/questions/7519790/assign-multiple-new-variables-in-a-single-line-in-r
#' @export
vassign <- function(..., values, envir=parent.frame()) {
  vars <- as.character(substitute(...()))
  values <- rep(values, length.out=length(vars))
  for(var in vars) {
    assign(var, values[[i]], envir)
  }
}
