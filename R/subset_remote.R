#' @title Returns the specific package from Remotes
#' @description Subsets the remotes based on a package
#'
#' @param path Path to DESCRIPTION file
#' @param package Package to subset
#' @return Character vector of remotes
#' @export
#'
#' @examples
#' path = example_description_file()
#' desc = get_remote_info(path)
subset_remote = function(
  path = "DESCRIPTION",
  package = "ANTsR") {

  parsed = remote_package_names(path = path)
  remotes = names(parsed)
  if (!is.null(package)) {
    remotes = remotes[(parsed %in% package)]
  }
  return(remotes)
}