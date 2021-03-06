% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functional.R
\name{And}
\alias{And}
\alias{Or}
\title{Predicates}
\usage{
And(...)

Or(...)
}
\arguments{
\item{...}{Predicate functions}

\item{...}{Predicate functions}
}
\description{
Predicates
}
\details{
\code{And} returns a function that returns \code{TRUE} when all the arguments,
applied to the returned function's arguments, returns \code{TRUE}.

\code{Or} returns a function that returns \code{TRUE} when any the arguments,
applied to the returned function's arguments, returns \code{TRUE}.
}
\seealso{
Other Functionals: \code{\link{Call}},
  \code{\link{Compose}}, \code{\link{Delay_by}},
  \code{\link{Dot_every}}, \code{\link{FMap}},
  \code{\link{Fail_with}}, \code{\link{Log_to}},
  \code{\link{Maybe}}, \code{\link{Partial}}
}
