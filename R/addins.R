#' Insert \%do\%.
#'
#' Call this function as an addin to insert \code{ \%do\% } at the cursor position.
#'
#' @export
insertDoAddin <- function() {
  stopifnot(requireNamespace("rstudioapi", quietly = TRUE))
  rstudioapi::insertText(" %do% ")
}

#' Insert \%||\%.
#'
#' Call this function as an addin to insert \code{ \%||\% } at the cursor position.
#'
#' @export
insertNCAddin <- function() {
  stopifnot(requireNamespace("rstudioapi", quietly = TRUE))
  rstudioapi::insertText(" %||% ")
}
