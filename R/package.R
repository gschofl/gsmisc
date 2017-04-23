#' @include project.R
NULL

#' Create a new package
#' 
#' \code{createPackage} is a simple wrapper around \code{\link[devtools]{create}}.
#' 
#' @param name Name of the package.
#' @param path Location of the package.
#' @param use_rcpp Add infrastructure for using \code{Rcpp}.
#' @param use_vignette Add a vignette template to the project.
#' @return Called for its side effects. Opens a new instance of Rstudio in
#'   the newly created package directory.
#' @export
#' @examples
#' \dontrun{
#'    createPackage("utils", use_rcpp = TRUE)  
#' }
#' 
createPackage <- function(name, path = getOption("gsmisc.devel"),
                          use_rcpp = FALSE, use_vignette = FALSE) {
  pkg <- normalizePath(file.path(path, name), mustWork = FALSE)
  devtools::create(pkg, rstudio = TRUE)
  devtools:::use_git_ignore("scratch.[rR]", pkg = pkg, quiet = TRUE)
  ## add scratch file
  scratch.file <- system.file(file.path("defaults", "config", "template.scratch"), package = "gsmisc")
  file.copy(scratch.file, file.path(pkg, "scratch.R"), overwrite = FALSE)
  devtools::use_package_doc(pkg)
  devtools::use_testthat(pkg)
  devtools::use_mit_license(pkg)
  devtools::use_readme_rmd(pkg)
  #devtools::use_news_md(pkg)
  if (use_rcpp) {
    devtools::use_rcpp(pkg)
  }
  if (use_vignette) {
    devtools::use_vignette(name, pkg)
  }
  rproject <- file.path(pkg, paste0(name, ".Rproj"))
  open_rstudio_project(rproject)
}
