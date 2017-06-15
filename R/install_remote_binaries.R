#' @title Install Binaries from Remotes
#' @description Gets Remotes, gets Binaries form GH, and installs them, then
#' drops them from Remotes field
#'
#' @param path Path to DESCRIPTION file
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
  package = NULL,
  drop_all = TRUE,
  verbose = TRUE
) {
  urls = remote_binaries(path = path)
  if (!is.null(package)) {
    if (package != "") {
      urls = urls[intersect(package, names(urls))]
    }
  }
  if (length(urls) > 0) {
    packs = names(urls)
    if (verbose) {
      message(paste0("Installing ", paste(packs, collapse = ","),
                     " package binaries"))
    }
    urls = unlist(urls)
    destfiles = file.path(tempdir(), basename(urls))
    files = mapply(function(url, destfile){
      download.file(url, destfile, method = "wget",
                    quiet = TRUE)
    }, urls, destfiles)
    files = NULL
    install.packages(destfiles,
                     repos = NULL,
                     type = .Platform$pkgType)
    if (!drop_all) {
      packs = packs[ packs %in% installed.packages()]
    }
    drop_remotes(path = path, drop_remotes = packs)
  }
  return(NULL)
}
