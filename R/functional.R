#' @include compact.R
NULL

# Much of the code in here is taken from Hadley Wickham's
# "Advanced R Programming" (http://adv-r.had.co.nz/)

#' Call a function with arguments provided individually.
#' 
#' @param fn The function to call.
#' @param \dots Arguments to function \code{fn}.
#' @return The return value of the function call.
#' @family Functionals
#' @export
#' @examples
#' funs <- list("mean", "sd", "var")
#' sapply(funs, Call, 1:100)
#'
#' ## invoke an anonymous function
#' Call(function(a, b) a*b, 3, 4)
Call <- function(fn, ...) {
  fn <- match.fun(fn)
  fn(...)
}

#' Compose multiple functions.
#'
#' Returns a function that applies the last argument to its input, than
#' the penultimate argument and so on.
#'
#' @param ... The functions to be composed.
#' @param f,g Two functions to compose (infix notation).
#' @family Functionals
#' @export
#' @examples
#'  x <- c(1, 1, 2, 2, 3, 3)
#'  nunique <- Compose(length, unique)
#'  nunique(x) == length(unique(x))
#'  
#'  ## useful in lapply constructs
#'  sapply(mtcars, length %.% unique)
Compose <- function(...) {
  fns <- lapply(compact(list(...)), match.fun)
  len <- length(fns)
  head <- fns[[len]]
  tail <- fns[-len]
  function(...) {
    rs <- head(...)
    for (fn in rev(tail)) {
      rs <- fn(rs)
    }
    rs
  }
}

#' @rdname Compose
#' @export
"%.%" <- function(f, g) {
  f <- match.fun(f)
  g <- match.fun(g)
  function(...) f(g(...))
}

#' @details \code{Sequence} is the same as \code{Compose}, except that it
#' applies the functions in argument-list order.
#' @rdname Compose
#' @export
Sequence <- function(...) {
  fns <- lapply(compact(list(...)), match.fun)
  head <- fns[[1L]]
  tail <- fns[-1L]
  function(...) {
    rs <- head(...)
    for (fn in tail) {
      rs <- fn(rs)
    }
    rs
  }
}

#' Partial function application.
#'
#' Modify a function by pre-applying some of the arguments.
#'
#' @param .fn Function to apply partially.
#' @param ... Named arguments that should be applied to \code{.fn}
#' @param .env the environment of the created function. Defaults to
#'   \code{\link{parent.frame}}.
#' @param .lazy If \code{TRUE} arguments are evaluated lazily.
#' @return A function.
#' @family Functionals
#' @export
#' @examples
#' mean1 <- Partial(mean, na.rm = TRUE)
#' mean1(c(1, 2, 3, NA, 4))
Partial <- function(.f, ..., .env = parent.frame(), .lazy = TRUE) {
  stopifnot(is.function(.f))
  fcall <- if (.lazy) {
    substitute(.f(...))
  } else {
    as.call(c(substitute(.f), list(...)))
  }
  fcall[[length(fcall) + 1L]] <- quote(...)
  args <- list("..." = quote(expr = ))
  eval(call("function", as.pairlist(args), fcall), .env)
}

#' Fast Map.
#' 
#' A thin wrapper around internal \code{\link{mapply}}, which can be
#' a bit faster than base \code{\link{Map}}.
#' 
#' @param fn Function to apply.
#' @param \dots Arguments to \code{fn}; Vectors or lists.
#' @return A list.
#' @export
#' @family Functionals
#' @examples
#' require(microbenchmark)
#' microbenchmark(
#' mapply(`*`, 1:100, 101:200),
#' Map(`*`, 1:100, 101:200),
#' FMap(`*`, 1:100, 101:200),
#' (1:100)*(101:200))
FMap <- function(fn, ...) {
  fn <- match.fun(fn)
  .mapply(fn, list(...), MoreArgs = NULL)
}


#' Maybe call a function
#' 
#' If the argument(s) to a function are missing or \code{NULL},
#' return \code{NULL}; otherwise apply the function.
#' 
#' @param fn A function.
#' @return A function.
#' @family Functionals
#' @export
Maybe <- function(fn) {
  function(x, ...) {
    if (missing(x) || is.null(x))
      return(NULL)
    fn(x, ...)
  }
}

#' Fail with a default value
#' 
#' @param default Default value.
#' @param fn A function
#' @param verbose Show error message
#' @family Functionals
#' @export
Fail_with <- function(default = NULL, fn, verbose = TRUE) {
  fn <- match.fun(fn)
  function(...) {
    out <- default
    tryCatch(out <- fn(...), error = function(e) {
      if (verbose) {
        message("Error caught: ", sQuote(e$message))
      } else {
        
      }
    })
    out
  }
}

#' Delay function call
#' 
#' @param delay delay in seconds.
#' @param fn function to call
#' @family Functionals
#' @export
Delay_by <- function(delay, fn) {
  fn <- match.fun(fn)
  function(...) {
    Sys.sleep(delay)
    fn(...)
  }
}

#' Print a dot ever nth function call
#' 
#' @param n when to print a dot
#' @param fn function call
#' @family Functionals
#' @export
Dot_every <- function(n, fn) {
  i <- 1
  fn <- match.fun(fn)
  function(...) {
    if (i %% n == 0) {
      cat('.')
    }
    i <<- i + 1
    fn(...)
  }
}

#' Log a time stamp and a message to file everytime a function is run
#' 
#' @param path path to log file
#' @param message logging message
#' @param fn function
#' @family Functionals
#' @export
Log_to <- function(path, message="", fn) {
  fn <- match.fun(fn)
  now <- function(accuracy = 4) {
    paste0("-- ", format(Sys.time(), paste0("%M:%OS", accuracy)), " -- ")
  }
  if (missing(path) || is.null(path)) {
    function(...)
      fn(...)
  } else {
    assert_that(file.exists(path))
    function(...) {
      cat(now(), message, sep="", file=path, append=TRUE)
      fn(...)
    }
  }
}

#' Predicates
#' 
#' @details
#' \code{And} returns a function that returns \code{TRUE} when all the arguments,
#' applied to the returned function's arguments, returns \code{TRUE}.
#' 
#' \code{Or} returns a function that returns \code{TRUE} when any the arguments,
#' applied to the returned function's arguments, returns \code{TRUE}.
#' 
#' @param ... Predicate functions
#' @rdname Predicates
#' @family Functionals
#' @export
And <- function(...) {
  fns <- lapply(compact(list(...)), match.fun)
  function(...) {
    value <- TRUE
    for (fn in fns)
      if (!(value <- fn(...)))
        break
    value
  }
}

#' @param ... Predicate functions
#' @rdname Predicates
#' @export
Or <- function(...) {
  fns <- lapply(compact(list(...)), match.fun)
  function(...) {
    value <- FALSE
    for (fn in fns)
      if (value <- fn(...))
        break
    value
  }
}
