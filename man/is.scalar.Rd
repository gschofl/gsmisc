\name{is.scalar}
\alias{all_scalar}
\alias{are_scalar}
\alias{is.scalar}
\title{Is an object of length one?}
\usage{
  is.scalar(x)

  are_scalar(x)

  all_scalar(x)
}
\arguments{
  \item{x}{object to test}
}
\description{
  Which elements in a list are of length one? Are all
  elements in a list of length one?
}
\examples{
is.scalar(1)
is.scalar(NULL)
are_scalar(list(1,2,3))
are_scalar(c(1,2,3))
all_scalar(list("a", "b", c("c", "d")))
}
\seealso{
  Other tests: \code{\link{all_empty}},
  \code{\link{are_empty}}, \code{\link{has_command}},
  \code{\link{is.empty}}
}
