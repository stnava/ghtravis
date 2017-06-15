#' @title Install Binaries from Remotes
#' @description Gets Remotes, gets Binaries form GH, and installs them, then
#' drops them from Remotes field
#'
#' @param path Path to DESCRIPTION file,
#' passed to \code{\link{remote_binaries}}
#' @param remotes Remotes to get binaries for - in case not going from DESCRIPTION,
#' passed to \code{\link{remote_binaries}}
#' @param package Specific package to install
#' @param drop_all If TRUE, then will only drop packages that are installed
#' @param verbose Print diagnostic messages
#'
#' @return Invisible NULL
#' @export
#'
#' @examples \dontrun{
#' path = example_description_file()
#' install_remote_binaries(path = path)
#' }
#' @importFrom utils install.packages installed.packages
install_remote_binaries = function(
  path = "DESCRIPTION",
  remotes = NULL,
  package = NULL,
  drop_all = TRUE,
  verbose = TRUE
) {
  urls = remote_binaries(path = path, remotes = remotes)
  if (!is.null(package)) {
    if (package != "") {
      urls = urls[intersect(package, names(urls))]
    }
  }
  if (length(urls) > 0) {
    packs = names(urls)
    urls = unlist(urls)
    for (ipack in seq_along(urls)) {
      pack = packs[ipack]
      url = urls[ipack]
      if (verbose) {
        message(paste0("Installing ", pack,
                       " package binaries"))
      }
      # run_url =
      destfile = file.path(tempdir(), basename(url))

      # files = mapply(function(url, destfile){
      download.file(url, destfile, method = "wget",
                    quiet = !verbose)
      # }, urls, destfiles)
      # files = NULL
      install.packages(destfile,
                       repos = NULL,
                       type = .Platform$pkgType)
    }
    if (!drop_all) {
      packs = packs[ packs %in% installed.packages()]
    }
    drop_remotes(path = path, drop_remotes = packs)
  }
  return(NULL)
}
