#' @importFrom xtable xtable print.xtable
NULL

#' Generate and print a LaTeX or HTML table.
#'
#' A function that wraps \code{\link[xtable]{xtable}} and \code{\link[xtable]{print.xtable}},
#' to make it easy to use in "Rmarkdown" dokuments.
#'
#' @details The output style (LaTeX or HTML) is controlled via the option
#' \code{options(xtable.type = "latex")} or \code{options(xtable.type = "html")}
#' 
#' @inheritParams xtable::xtable
#' @return Prints the table to stdout.
#' @export
latex <- function(df, caption = NULL, label = NULL, align = NULL, digits = NULL,
                  display = NULL, ...) {
  tbl <- xtable(df, caption = caption, label = label, align = align,
                digits = digits, display = display)
  print.xtable(tbl, ..., comment = FALSE)
}

