#' @title Returns the specific package from Remotes
#' @description Subsets the remotes based on a package
#'
#' @param path Path to DESCRIPTION file
#' @param package Package to subset.  If NULL, then all will be installed
#' @param reorder should remotes be reordered before running
#' @param drop Remotes should be dropped after installing
#' @param ... arguments to pass to install_github
#' @return Character vector of remotes
#'
#' @importFrom devtools install_github
#' @return Output of \code{\link{install_github}}, indicator
#' of TRUE/FALSE for installation
#' @export
#'
#' @examples \dontrun{
#'   install_remotes_no_dep()
#' }
install_remotes_no_dep = function(
  path = "DESCRIPTION",
  package = NULL,
  reorder = TRUE,
  drop = TRUE,
  ...) {
  if (!is.null(package)) {
    if (all(package == "")) {
      package = NULL
    }
  }
  if (reorder) {
    reorder_remotes(path = path)
  }
  remotes = subset_remote(path = path, package = package)
  if (length(remotes) == 0) {
    return(NULL)
  }
  if (all(remotes == "")) {
    return(NULL)
  }
  if (drop) {
    drop_remotes(path = path, drop_remotes = remotes)
  }
  remotes = remotes[ !(remotes %in% "") ]
  res = sapply(remotes, devtools::install_github,
         auth_token = github_pat(quiet = TRUE),
         upgrade_dependencies = FALSE, ...)
  names(res) = remotes
  return(res)
}