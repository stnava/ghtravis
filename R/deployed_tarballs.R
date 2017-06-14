#' @title Deployed Tarball Version
#' @description Downloads a source tarball from GitHub and parses the
#' version in that source
#'
#' @param repo GitHub repository name
#' @param tag Tag for the tarball
#' @param version version of package tarball if applicable
#'
#' @return Version number character vector
#' @export
#'
#' @examples
#' deployed_tarball_version(  repo = "stnava/ITKR", tag = "latest")
deployed_tarball_version = function(
  repo = "stnava/ITKR",
  tag = "latest"
) {
  repo = parse_remotes(x = repo)
  repo = sapply(repo, function(x) {
    paste0(x$username, "/", x$repo)
  })
  # bash workaround
  url = "https://github.com/"
  url = paste0(url, repo,
               "/archive/",
               tag,
               ".tar.gz")
  # cat(paste0("Source tarball is: ", url))
  destfile = tempfile(fileext = ".tar.gz")
  dl = download.file(url, destfile = destfile)
  if (dl != 0) {
    return(NULL)
  }
  files = untar(destfile,
                list = TRUE,
                exdir = tempdir())
  dcf = files[ toupper(basename(files)) == "DESCRIPTION" ]
  if (length(dcf) != 1) {
    return(NULL)
  }
  untar(destfile, files = dcf, exdir = tempdir())
  dcf = file.path(tempdir(), dcf)
  version = dcf_package_version(dcf)
  return(version)
}



#' @export
deployed_tarball = function(
  repo = "stnava/ITKR",
  tag = "latest",
  version = NULL
) {
  # bash workaround
  if (!is.null(version)) {
    if (version == "") {
      version = NULL
    }
  }

  if (is.null(version)) {
    version = deployed_tarball_version(
      repo = repo, tag = tag)
  }
  if (is.null(version)) {
    stop("Version is not correctly parsed!")
  }
  # bash workaround
  ext = sys_ext()

  package = sapply(repo, function(x) {
     x$repo
  })
  url = "https://github.com/"
  url = paste0(url, repo,
               "/releases/download/",
               tag,
               "/", package, "_", version,
               ext)
  return(url)
}
