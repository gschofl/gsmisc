#' @include operators.R
#' @importFrom assertthat on_failure "on_failure<-"
NULL

#' Is an object of length one?
#'
#' Which elements in a list are of length one?
#' Are all elements in a list of length one?
#'
#' @param x An object to test.
#' @family Tests
#' @rdname is.scalar
#' @export
#' @examples
#' is.scalar(1)
#' is.scalar(NULL)
is.scalar <- function(x) {
  length(x) == 1L
}
on_failure(is.scalar) <- function(call, env) {
  paste0(deparse(call$x), " does not have length one.")
}

#' @rdname is.scalar
#' @export
#' @examples
#' are_scalar(list(1, 2, 3))
#' are_scalar(c(1, 2, 3))
are_scalar <- function(x) {
  stopifnot(!is.null(x))
  vapply(x, is.scalar, FALSE, USE.NAMES = FALSE)
}

#' @rdname is.scalar
#' @export
#' @examples
#' all_scalar(list("a", "b", c("c", "d")))
all_scalar <- function(x) all(are_scalar(x))
on_failure(all_scalar) <- function(call, env) {
  paste0("Not all elements in ", deparse(call$x), " are of length one.")
}

#' @rdname is.scalar
#' @export
is.string <- function(x) {
  is.character(x) && is.scalar(x)
}
on_failure(is.string) <- function(call, env) {
  paste0(deparse(call$x), " is not a character string.")
}

#' Is an object empty?
#'
#' Which elements in a list are empty?
#' Are all elements in a list empty?
#' 
#' @param x An object to test.
#' @return A logical.
#' @rdname is.empty
#' @family Tests
#' @export
#' @examples
#' is.empty(NULL)
#' is.empty(numeric())
#' is.empty(list())
#' is.empty("")
is.empty <- function(x) {
  is.null(x) || length(x) == 0L || ( is.scalar(x) && !nzchar(x) )
}
on_failure(is.empty) <- function(call, env) {
  paste0(deparse(call$x), " is not empty.")
}

#' @rdname is.empty
#' @export
#' @examples
#' are_empty(list(1, NULL, 3, NA))
are_empty <- function(x) {
  if (is.recursive(x) || length(x) > 1) {
    vapply(x, function(x) is.null(x) || length(x) == 0L, FALSE, USE.NAMES = FALSE) | !nzchar(x)
  } else {
    is.empty(x)
  }
}

#' @rdname is.empty
#' @export
#' @examples
#' all_empty(NULL)
#' all_empty(list(NULL, NULL, character()))
all_empty <- function(x) all(are_empty(x))
on_failure(all_empty) <- function(call, env) {
  paste0("Not all elements in ", deparse(call$x), " are empty.")
}

#' Test if an external executable is available.
#' 
#' Uses \code{\link{Sys.which}} internally, so it should work
#' on Windows and Unix.alikes.
#' 
#' @param cmd An exececutable to test for.
#' @param msg Additional message if the test fails.
#' @return A logical.
#' @family Tests
#' @export
has_command <- function(cmd, msg = "") {
  assert_that(is.string(cmd))
  unname(Sys.which(cmd) != "")
}
on_failure(has_command) <- function(call, env) {
  paste0("Dependency ", sQuote(eval(call$cmd, env)), " is not installed\n",
         eval(call$msg, env))
}

#' Which elements in a list are NULL?
#'
#' @param x An object to test.
#' @param A logical vector.
#' @family Tests
#' @export
#' @examples
#' are_null(list(1, NULL, 3))
are_null <- function(x) {
  stopifnot(is.list(x))
  vapply(x, is.null, FALSE, USE.NAMES = FALSE)
}

#' Which elements in a list are TRUE?
#'
#' @param x An object to test.
#' @param A logical vector.
#' @family Tests
#' @export
#' @examples
#' are_true(list(FALSE, TRUE, TRUE))
are_true <- function(x) {
  assert_that(is.list(x))
  vapply(x, isTRUE, FALSE, USE.NAMES = FALSE)
}

#' Which elements in a list are FALSE?
#'
#' @param x An object to test.
#' @return A logical vector.
#' @family Tests
#' @export
#' @examples
#' are_false(list(FALSE,TRUE,TRUE))
are_false <- function(x) {
  assert_that(is.list(x))
  vapply(x, function(x) identical(x, FALSE), FALSE, USE.NAMES = FALSE)
}

#' Is an R Packages installed?
#'
#' @param pkg Package name as character string.
#' @return A logical.
#' @family Tests
#' @export
#' @examples
#' is.installed("methods")
is.installed <- function(pkg) {
  stopifnot(is.string(pkg))
  is.element(pkg, .packages(all.available = TRUE))
}
on_failure(is.installed) <- function(call, env) {
  paste0("Package ", deparse(call$pkg), " is not installed.")
}
