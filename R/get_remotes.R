#' @title Get the Remotes from a Package
#' @description Parse a DESCRIPTION file and returns the remotes
#'
#' @return Character vector of remotes
#' @export
#' @examples
#' path = example_description_file()
#' get_remotes(path)
get_remotes = function(path = "DESCRIPTION"){
  if (is.character(path)) {
    path = read_dcf(path)
    path = path$dcf
  }
  remotes = path$Remotes[[1]]
  remotes = trimws(remotes)
  remotes = split_remotes(remotes)
  if (is.null(remotes)) {
    remotes = ""
  }
  return(remotes)
}