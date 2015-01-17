#' Plot the class inheritance graph
#' 
#' This function is taken from the help page for \code{\link[methods]{classesToAM}}.
#' 
#' @param classes A character vector of class names.
#' @param subclasses A logical flag. Include all known subclasses.
#' @param \dots Further arguments.
#' @return Plots a directed graph of class inheritance, and returns it invisibly
#' @export
#' @examples
#' \dontrun{
#' plotInheritance(getClasses("package:Rentrez"))
#' }
plotInheritance <- function(classes, subclasses = FALSE, ...) {
  if (!requireNamespace("Rgraphviz", quietly = TRUE)) {
    stop("Please install Rgraphviz", call. = FALSE)
  }
  mm <- classesToAM(classes, subclasses)
  classes <- rownames(mm)
  rownames(mm) <- colnames(mm)
  graph <- new("graphAM", mm, "directed", ...)
  plot(graph)
  cat("Key:\n", paste0(abbreviate(classes), " = ", classes, ", "),
      sep = "", fill = TRUE)
  invisible(graph)
}

#' Use Google Search from the R command line.
#' 
#' @param query Search query
#' @param n Max number of results returned.
#' @return Prints the URLs returned by Google Search to screen and invisibly
#' returns a character vector of the URLs. 
#' @export
#' @examples
#' r <- google("use google from command line")
#' \dontrun{
#' browseURL(r[1])
#' }
google <- function(query = "", n = 10) {
  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop("Please install RCurl", call. = FALSE)
  }
  h <- basicTextGatherer()
  url <- paste0("https://www.google.de/search?tbs=li:1&q=", curlEscape(query))
  curlPerform(url = url, writefunction = h$update, .opts = list(timeout = 6))
  ## Print the matching parts on a separate output line each
  stream <- gsub("&amp", "", gsub("/url?q=", "", system("grep -oP '\\/url\\?q=.+?&amp'", input = h$value(), intern = TRUE), fixed = TRUE))
  if (length(stream) > n)
    stream <- stream[1:n]
  print(stream)
  return(invisible(stream))
}

#' Show head and tail of a data structure.
#' 
#' Prints head and tail of avector, list, matrix, or data.frame.
#' 
#' @param x An object.
#' @param n How many elements to print.
#' @return Only called for its side effect.
#' @seealso \code{\link{head}}, \code{link{tail}}.
#' @export
ht <- function(x, n = 6) {
  print(head(x, n = n))
  cat(' .\n .\n .\n')
  print(tail(x, n = n))
  invisible()
}




