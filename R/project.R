#' @include functional.R
NULL

#' Open an RStudio project from R
#' 
#' Quickly open a package (ar any  directory in the search path) as an RStudio
#' project. The search paths are defined by the custom options \code{gsmisc.pkgs},
#' \code{gsmisc.proj}, and \code{gsmisc.devel}, which I have preset in \code{.Rprofile}.
#' If none of these is defined \code{rproj} will fall back to the current working
#' directory.
#'  
#' @param pkg The name of a package (or project directory) given as a Symbol.
#' @param path One of 'all', 'pkgs', 'proj', or 'devel'.
#'  
#' @return Opens RStudio.
#' @seealso Inspired by \href{http://stackoverflow.com/questions/18426726/system-open-rstudio-close-connection}{this} question on stackoverflow
#' @export  
rproj <- function(pkg, path = 'all') {
  open_project <- function(rproj) {
    rstudio <- Sys.which("rstudio")
    if (rstudio == "") {
      stop("RStudio is not installed in PATH", call.=TRUE)
    }
    action <- paste(rstudio, rproj)
    system(action, wait=FALSE, ignore.stderr=TRUE)
  }
  "%||%" <- function (a, b) {
    if (is.null(a)) b else a
  }
  Rproj.template <- c("Version: 1.0", "", "RestoreWorkspace: Default",
                      "SaveWorkspace: Default",  "AlwaysSaveHistory: Default",
                      "", "EnableCodeIndexing: Yes",  "UseSpacesForTab: Yes",
                      "NumSpacesForTab: 2", "Encoding: UTF-8",  "",
                      "RnwWeave: knitr", "LaTeX: pdfLaTeX")

  if (is.name(pkg)) {
    pkg <- gsub("^\"|\"$", '', deparse(substitute(pkg)))
  } else if (is.character(pkg) && length(pkg) == 1) {
    pkg <- pkg
  } else {
    stop("'pkg' must be a symbol or a string")
  }
  devel.path <- getOption('gsmisc.devel') %||% '.'
  proj.path  <- getOption('gsmisc.proj') %||% '.'
  pkgs.path  <- getOption('gsmisc.pkgs') %||% '.'
  path <- switch(path,
                 all = normalizePath(unique(c(devel.path, proj.path, pkgs.path))),
                 devel = normalizePath(devel.path),
                 proj = normalizePath(proj.path),
                 pkgs = normalizePath(pkgs.path),
                 normalizePath(path, mustWork=TRUE))
  
  pkg_path <- grep(pkg, dir(path, full.names=TRUE, ignore.case=TRUE), value=TRUE)
  
  while (length(unique(basename(pkg_path))) > 1L) {
    pkg_path <- unique(dirname(pkg_path))
  }
  
  if (length(pkg_path) > 1) {
    warning("Found ", length(pkg_path), " packages of  name ", sQuote(pkg), ".\n",
            "Will open the first: ", sQuote(pkg_path[1]), call.=FALSE, immediate.=TRUE)
    pkg_path <- pkg_path[1]
  } else if (length(pkg_path) < 1) {
    stop("Package ", sQuote(pkg), " not found.", call.=FALSE)
  }
  
  rproj_loc <- dir(pkg_path, pattern="*.Rproj", full.names=TRUE)
  if (length(rproj_loc) < 1) {
    rproj_loc <- file.path(pkg_path, paste0(pkg, '.Rproj'))
    cat(paste(Rproj.template, collapse = "\n"), file = rproj_loc)  
  }
  
  open_project(rproj_loc)
}


#' Create a modified \href{http://projecttemplate.net/getting_started.html}{ProjectTemplate}
#' project.
#' 
#' @param project A character vector with the name of the project directory.
#' @param path The base directory of the project.
#' @param merge_strategy What should happen if the target directory exists and
#'   is not empty?
#'   If \code{"force.empty"}, the target directory must be empty;
#'   if \code{"allow.non.conflict"}, the method succeeds if no files or
#'   directories with the same name exist in the target directory.
#' @param open If \code{TRUE}, open the newly created project in RStudio.
#' @param packrat If \code{TRUE}, create a \href{http://rstudio.github.io/packrat/walkthrough.html}{packrat} project.
#' @return No value is returned; this function is called for its side effects.
#' @details  If the target directory does not exist, it is created.  Otherwise,
#'   it can only contain files and directories allowed by the merge strategy.
#' @note This function is slightly modified code taken from the
#'   \code{\link[ProjectTemplate]{create.project}} function in John Myles White's
#'   \code{ProjectTemplate} package to allow me using my custom directory structure.
#' @export
createProject <- function(project = 'myProject',
                          path = getOption("gsmisc.proj"),
                          merge_strategy = c("require.empty", "allow.non.conflict"),
                          open = TRUE,
                          packrat = FALSE) {
  stopifnot(require('ProjectTemplate'), !missing(project))
  project_name <- normalizePath(file.path(path, project), mustWork = FALSE)
  assert_that(is.writeable(dirname(project_name)))
  template_name <- 'template'
  temp_dir <- tempfile("ProjectTemplate")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  untar(system.file(file.path("defaults", paste0(template_name, ".tar")), package = "gsmisc"),
        exdir = temp_dir, tar = "internal")
  template_path <- file.path(temp_dir, template_name)
  
  merge_strategy <- match.arg(merge_strategy)
  if (file.exists(project_name) && file.info(project_name)$isdir) {
    .create_existing_project(template_path, project_name, merge_strategy)
  } else {
    .create_new_project(template_path, project_name)
  }
  if (packrat) {
    stopifnot(require("packrat", character.only = TRUE))
    packrat::init(project = project_name)
  }
  if (open) {
    rproj(eval(project), path = path)
  }
}


.create_existing_project <- function(template.path, project.name, merge.strategy) {
  template.files <- ProjectTemplate:::.list.files.and.dirs(path = template.path)
  project.path <- file.path(project.name)
  switch(
    merge.strategy,
    require.empty = {
      if (!ProjectTemplate:::.dir.empty(project.path))
        stop(paste("Directory", project.path,
                   "not empty.  Use merge.strategy = 'allow.non.conflict' to override."))
    },
    allow.non.conflict = {
      target.file.exists <- file.exists(file.path(project.path, template.files))
      if (any(target.file.exists))
        stop(paste("Creating a project in ", project.path,
                   " would overwrite the following existing files/directories:\n",
                   paste(template.files[target.file.exists], collapse=', ')))
    },
    stop("Invalid value for merge.strategy:", merge.strategy))
  
  file.copy(from = file.path(template.path, template.files),
            to = project.path,
            recursive = TRUE, overwrite = FALSE)
  
  file.copy(from = system.file('defaults/config/global.dcf', package = 'gsmisc'),
            to = file.path(project.path, 'config/global.dcf'))

  file.copy(from = system.file('defaults/config/template.rproject', package = 'gsmisc'),
            to = file.path(project.path, paste0(basename(project.path), '.Rproj')))
  
  file.copy(from = system.file('defaults/config/template.Rmd', package = 'gsmisc'),
            to = file.path(project.path, 'reports/report.Rmd'))
  
  file.copy(from = system.file('defaults/config/custom.css', package = 'gsmisc'),
            to = file.path(project.path, 'reports/custom.css'))
}

.create_new_project <- function(template.path, project.name) {
  if (file.exists(project.name)) {
    stop(paste("Cannot run create.project() from a directory containing", project.name))
  }
  
  dir.create(project.name)
  tryCatch(
    .create_existing_project(template.path = template.path,
                             project.name = project.name,
                             merge.strategy = "require.empty"),
    error = function(e) {
      unlink(project.name, recursive = TRUE)
      stop(e)
    }
  )
}
