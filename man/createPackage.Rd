% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\name{createPackage}
\alias{createPackage}
\title{Create a new package}
\usage{
createPackage(name, path = getOption("gsmisc.devel"), use_rcpp = FALSE,
  use_vignette = FALSE)
}
\arguments{
\item{name}{Name of the package.}

\item{path}{Location of the package.}

\item{use_rcpp}{Add infrastructure for using \code{Rcpp}.}

\item{use_vignette}{Add a vignette template to the project.}
}
\value{
Called for its side effects. Opens a new instance of Rstudio in
  the newly created package directory.
}
\description{
\code{createPackage} is a simple wrapper around \code{\link[devtools]{create}}.
}
\examples{
\dontrun{
   createPackage("utils", use_rcpp = TRUE)  
}

}
