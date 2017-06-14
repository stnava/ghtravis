# gist: 69c48f469ab34cc05752361645ffa8e0
rewrite_dcf = function(
  path = "DESCRIPTION",
  drop_remotes = c("ANTsR", "ITKR")) {

  # remotes = get_remotes(path )
  rres = read_dcf(path)
  res = rres$dcf
  # nres = names(res)
  remotes = get_remotes(res)

  parsed = parse_remotes(remotes)
  pack_with_remote = sapply(parsed, function(x) {
    x$repo
  })
  remotes = remotes[!(pack_with_remote %in% drop_remotes)]
  remotes = paste(remotes, collapse = ", ")

  res$Remotes = remotes
  nres = names(res)
  res = as.data.frame(res,
                      stringsAsFactors = FALSE)
  names(res) = nres

  write.dcf(x = res,
            file = path)
}

install_remotes_no_dep = function(
  path = "DESCRIPTION", ...) {
  rres = read_dcf(path)
  res = rres$dcf
  # nres = names(res)
  remotes = get_remotes(res)
  if (length(remotes) == 0) {
    return(NULL)
  }
  if (remotes == "") {
    return(NULL)
  }
  lapply(remotes, devtools::install_github,
         upgrade_dependencies = FALSE, ...)
}

install_remote_no_dep = function(path = "DESCRIPTION",
                                 package = "ANTsR", ...) {

  remote = subset_remote(path = path, package = package)
  if (length(remote) > 0) {
    devtools::install_github(remote,
                           upgrade_dependencies = FALSE, ...)
  }
}

############################################
# Edit travis for neuroconductor release deployment
############################################
deploy_travis = function(api_key,
                         path = "DESCRIPTION",
                         travis_file = ".travis.yml",
                         user = "neuroconductor"
) {
  if (!"yaml" %in% installed.packages()) {
    install.packages("yaml")
  }
  res = read_dcf(path)
  package = res$dcf$Package
  if (!file.exists(travis_file)) {
    stop("TRAVIS FILE NOT FOUND!")
  }
  trav = readLines(travis_file)
  trav = paste(trav, collapse = "\n")
  trav = yaml::yaml.load(string = trav)
  deploy = list(deploy = list(
    provider = "releases",
    skip_cleanup = TRUE,
    api_key = list(
      secure = api_key),
    file_glob = TRUE,
    file = paste0(package, "*.t*gz"),
    on = list(
      tags = TRUE, repo = paste0(user, "/", package)
    )
  ))
  trav$deploy = NULL
  trav = yaml::as.yaml(trav,
                       indent.mapping.sequence = TRUE)
  deploy = yaml::as.yaml(deploy)
  deploy = gsub("\n'on':\n", "\non:\n",
                deploy, fixed = TRUE)
  trav = paste(trav, deploy, sep = "\n")
  writeLines(trav, con = travis_file)
  return(travis_file)
}

############################################
# Construct the tarball filename
# with just a tag - downloads the source
############################################
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



##
# 1. Parse Remotes field
# 2. Pass user/package@CID to function
# 3. Function pull GH API for tag/commit pairs
# 4. Function pulls GH API for tag/release information
# 5. Function checks to binary is there, sorting decreasing by creation date
# 6. Function downloads/installs binaries & drops remote
# 7. If not remote is found, doesn't do anything, and keeps remote, and install_github
# should take care of the rest
# order may not matter?
# based on DESCRIPTION
# repo = "muschellij2/neurobase@asdf"



remote_binaries = function(remotes = NULL,
                           path = "DESCRIPTION") {
  if (is.null(remotes)) {
    remotes = get_remotes(path)
  }
  packs = parse_remotes(remotes)
  packs = sapply(packs, `[[`, "repo")
  urls = lapply(remotes, latest_release_with_binary)
  names(urls) = packs
  return(urls)
}

remote_binary = function(path = "DESCRIPTION", package = "ANTsRCore") {
  bins = remote_binaries(path = path)
  keep = package %in% names(bins)
  package = package[keep]
  if (any(keep)) {
    bins = bins[package]
    bins = unlist(bins)
    return(bins)
  } else {
    return("")
  }
}

install_remote_binaries = function(
  path = "DESCRIPTION",
  package = NULL
) {
  urls = remote_binaries(path = path)
  if (!is.null(package)) {
    urls = urls[intersect(package, names(urls))]
  }
  if (length(urls) > 0) {
    packs = names(urls)
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
    rewrite_dcf(path = path, drop_remotes = packs)
  }
}


install_remote_binaries = function(
  path = "DESCRIPTION",
  package = NULL
) {
  urls = remote_binaries(path = path)
  if (!is.null(package)) {
    urls = urls[intersect(package, names(urls))]
  }
  if (length(urls) > 0) {
    packs = names(urls)
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
    rewrite_dcf(path = path, drop_remotes = packs)
  }
}

