#' @include functional.R
NULL

bioc <- function() {
  source("http://bioconductor.org/biocLite.R")
  biocLite(pkgs = "BiocInstaller", suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
}

assure_bioc <- function() {
  if (!requireNamespace("BiocInstaller", quietly = TRUE)) {
    bioc()
  }
}

#' Install or update packages.
#' 
#' \code{install_packages} is a wrapper around \code{\link{biocLite}}.
#' In addition to the standard functionality provided \code{biocLite},
#' this function also downloads and untars the source files to \code{destdir}.
#' 
#' \code{update_packages} checks for outdated packages, save the source to
#' \code{destdir}, extract the packages in \code{destdir}, and installs them.
#' 
#' @param pkgs Packages
#' @param destdir Where to put the source code.
#' @param update call \code{\link{update_packages}}.
#' @param ... Further arguments passed on to \code{\link{biocLite}}.
#' @return Called for its side effects.
#' @family Installers
#' @export
install_packages <- function(pkgs, destdir = getOption("gsmisc.pkgs"), update = FALSE, ...) {
  assert_that(!missing(pkgs), is.character(pkgs))
  assure_bioc()
  BiocInstaller::biocLite(pkgs, suppressUpdates = TRUE, destdir = destdir, ...)
  
  if (update) {
    update_packages(destdir = destdir)
  } else if (!is.null(destdir)) {
    destdir <- normalizePath(destdir)
    cwd <- setwd(destdir)
    extract_packages(destdir)
    setwd(cwd)
  }
}

#' @rdname install_packages
#' @export
update_packages <- function(destdir = getOption("gsmisc.pkgs")) {
  if (!is.null(destdir)) {
    destdir <- normalizePath(destdir)
    cwd <- setwd(destdir)
    extract_packages(destdir)
  }
  tryCatch(biocValid(), error = function(e) {
    if (e$message %~% "package\\(s\\) out of date") {
      message("Updating ", strsplitN(e$message, " ", 1:2))
      ask <- if (interactive()) TRUE else FALSE
      biocLite(destdir = destdir, ask = ask)
      if (!is.null(destdir)) {
        extract_packages(destdir)
      }
    } else {
      message("Nothing to update")
    }
  })
           
  setwd(cwd)
}

#' @importFrom assertthat is.writeable
#' @keywords internal
extract_packages <- function(destdir) {
  assert_that(is.writeable(destdir))
  compressed <- dir(destdir, pattern = "gz$")
  if (any(duplicated(strsplitN(compressed, "_", 1)))) {
    stop("Duplicated gzip files in ", destdir, ". Resolve manually")
  }
  
  x <- lapply(compressed, untar)
  if (!all(unlist(x) == 0)) {
    stop("Failed to extract ", compressed[unlist(x) != 0])
  }
  
  x <- lapply(compressed, unlink)
  if (!all(unlist(x) == 0)) {
    stop("Failed to unlink ", compressed[unlist(x) != 0])
  }
  
  if (NROW(compressed)) {
    message("Extracted to ", destdir, ": ",
            paste0(compressed, collapse = ", "))
  }
}

#' Upgrade Bioconductor to the latest available version.
#' 
#' Downloads the latest version of the BiocInstaller package,
#' and upgrades all currently installed packages to the latest
#' repositories for this version of R.
#' 
#' @details 
#' Wraps \code{biocLite("BiocUpgrade")}
#' 
#' @export
upgrade_bioc <- function() {
  assure_bioc()
  BiocInstaller::biocLite("BiocUpgrade")
}

#' Install different sets of packages
#' 
#' Different sets of packages:
#' \code{base}, \code{datatools}, \code{hadleyverse}, \code{stats},
#' \code{phlyo}, \code{bioc}.
#' 
#' @param which Which set of packages do we want to install.
#' @export 
install_pkg_set <- function(which = "base", ...) {
  which <- match.arg(which, c("base", "datatools", "hadleyverse", "stats",
                              "phylo", "bioc"))
  which <- paste0("pkgs.", which)
  pkgs <- get(which, envir = asNamespace("gsmisc"))  
  install_packages(pkgs = pkgs, ...)
}

#' A quick installer for my R packages on GitHub
#' 
#' Prompts for which of the following packages you want to install:
#' \code{rmisc}, \code{Rentrez}, \code{biofiles}, \code{blastr},
#' \code{ncbi}.
#' @family Installers
#' @export
update_github <- function() {
  pkgs <- c("gsmisc", "reutils", "biofiles", "blastr", "ncbi", "genoslideR")
  cat("Update packages:\n")
  cat(sprintf("%s) %s", seq_along(pkgs), pkgs), sep = "\n")
  
  while (TRUE) {
    msg <- "Which packages do you want to upgrade? [a(ll)|12345|q(uit)]: "
    which <- tolower(strsplit(gsub("\\s+", "", readline(msg)), "")[[1]])
    if (!paste(which, collapse = "") %in% c("a", "all", "q", "quit")) {
      if (!any(is.na(idx <- suppressWarnings(as.numeric(which))))
          && max(idx) <= length(pkgs)
          && min(idx) > 0) {
        pkgs <- pkgs[idx]
        break
      } else {
        message("Please submit 'all', 'quit', or numbers between 1 and ", length(pkgs))
      }
    } else {
      if (which[1] == "q") 
        pkgs <- ""
      break
    }
  }
  
  if (!all_empty(pkgs)) {
    pkgs <- paste0("gschofl/", pkgs)
    install_github(pkgs)
  }
}

#' Require one or more packages
#' 
#' A simple wrapper for \code{\link[base]{require}} that allows
#' loading multiple packages. Issues a warning if a package
#' cannot be loaded.
#' 
#' @param ... Package names (as strings).
#' @seealso \code{\link[base]{require}}
#' @export
require.all <- function(...) {
  pkgs <- as.character(list(...))
  if (all_empty(pkgs)) {
    return(invisible())
  }
  success <- suppressMessages(suppressWarnings(
    unlist(lapply(pkgs, require, character.only = TRUE))
  ))
  if (!all(success)) {
    warning("Failed to load package(s):\n", paste(pkgs[!success], collapse = ", "),
            call. = FALSE)
  }
  return(invisible(success))
}

#' Load saved datasets
#' 
#' Reload \code{RData} datasets. Wrapper for \code{\link[base]{load}}
#' that attempts to load all datasets found in a directory.
#' 
#' @param cache Path to directory containing RData-files.
#' @param envir the environment where the data should be loaded.
#' @param verbose print item names during loading.
#' @return A character vector of the names of objects created.
#' @export
load.all <- function(cache = "./cache", envir = .GlobalEnv, verbose = FALSE) {
  rdata <- dir(cache, pattern = "rda(ta)?$", full.names = TRUE, ignore.case = TRUE)
  if (all_empty(rdata)) {
    return(invisible())
  }
  rcon <- lapply(normalizePath(rdata), gzcon %.% gzfile)
  on.exit(lapply(rcon, close))
  success <- lapply(rcon, function(con) {
    tryCatch(load(con, envir = envir, verbose = verbose), error = function(e) e )
  })
  if (any(idx <- vapply(success, is, "error", FUN.VALUE = logical(1)))) {
    warning("Failed to load:\n", paste(rdata[idx], collapse = "\n"), call. = FALSE)
    success <- success[!idx]
  }
  return(unlist(success))
}
