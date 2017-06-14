deployed_tarball_version = function(
  package = "ITKR",
  user = "stnava",
  tag = "latest"
) {
  # bash workaround
  url = "https://github.com/"
  url = paste0(url, user, "/", package,
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



############################################
# Construct the tarball filename
############################################
deployed_tarball = function(
  package = "ITKR",
  user = "stnava",
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
      package = package,
      user = user,
      tag = tag)
  }
  if (is.null(version)) {
    stop("Version is not correctly parsed!")
  }
  # bash workaround
  ext = sys_ext()

  url = "https://github.com/"
  url = paste0(url, user, "/", package,
               "/releases/download/",
               tag,
               "/", package, "_", version,
               ext)
  return(url)
}
