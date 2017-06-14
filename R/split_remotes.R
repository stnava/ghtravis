#' @title Remotes Splitter
#' @description Wrapper for splitting and trimming on spaces
#'
#' @param x Character vector of inputs
#'
#' @return Character vector with multiple elements
#' @export
#'
#' @examples
#' split_remotes(c("stnava/ANTsR, blah/package@asdf"))
split_remotes <- function(x) {
  trimws(unlist(strsplit(x, ",[[:space:]]*")))
}