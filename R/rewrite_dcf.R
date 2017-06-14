# gist: 69c48f469ab34cc05752361645ffa8e0

read_dcf <- function(path = "DESCRIPTION") {
  file = file(path)
  on.exit({
    close(file)
  })
  fields <- colnames(read.dcf(file))
  dcf = as.list(read.dcf(file,
                         keep.white = fields, all = TRUE)[1, ])
  return(list(fields = fields,
              dcf = dcf))
}



split_remotes <- function(x) {
  trimws(unlist(strsplit(x, ",[[:space:]]*")))
}

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

parse_one_remote <- function(x) {
  pieces <- strsplit(x, "::", fixed = TRUE)[[1]]

  if (length(pieces) == 1) {
    type <- "github"
    repo <- pieces
  } else if (length(pieces) == 2) {
    type <- pieces[1]
    repo <- pieces[2]
  } else {
    stop("Malformed remote specification '",
         x, "'", call. = FALSE)
  }
  fun <- tryCatch(get(
    paste0(tolower(type), "_remote"),
    envir = asNamespace("devtools"),
    mode = "function", inherits = FALSE),
    error = function(e) stop("Unknown remote type: ",
                             type, call. = FALSE)
  )

  fun(repo)
}

parse_remotes = function(x) {
  lapply(x, parse_one_remote)
}


get_remote_info = function(
  path = "DESCRIPTION") {

  remotes = get_remotes(path)
  remotes = split_remotes(remotes)
  if (length(remotes) == 0) {
    remotes = ""
  }

  parsed = parse_remotes(remotes)
  names(parsed) = remotes
  return(parsed)
}

remote_package_names = function(path = "DESCRIPTION") {
  parsed = get_remote_info(path = path)
  pack_with_remote = sapply(parsed, function(x) {
    x$repo
  })
  pack_with_remote
}

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
  devtools::install_github(remote,
                           upgrade_dependencies = FALSE, ...)
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

package_version = function(path = "DESCRIPTION"){
  res = read_dcf(path)
  res$dcf$Version
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
  version = package_version(dcf)
  return(version)
}


sys_ext = function(){
  os = Sys.info()
  os = os[["sysname"]]
  ext = switch(
    os,
    Linux = "_R_x86_64-pc-linux-gnu.tar.gz",
    Darwin = ".tgz"
  )
  return(ext)
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

latest_release_with_binary = function(repo){
  info = parse_one_remote(repo)
  user = info$username
  package = info$repo
  ref = info$ref
  repo = paste0(user, "/", package)
  ###############################
  # Get teh SHAs from the tags
  ###############################
  tag_url = paste0("https://api.github.com/repos/", repo, "/tags")
  tag_res = httr::GET(tag_url)
  tag_content = httr::content(tag_res)
  unlist_df = function(x) {
    x = unlist(x)
    x = as.data.frame(t(x), stringsAsFactors = FALSE)
  }
  ensure_colnames = function(x, cn) {
    sd = setdiff(colnames(x), cn)
    for (isd in sd) {
      x[, isd] = NA
    }
    sd = setdiff(cn, colnames(x))
    for (isd in sd) {
      x[, isd] = NA
    }
    return(x)
  }
  bind_list = function(L) {
    L = lapply(L, unlist_df)
    cn = sapply(L, colnames)
    cn = unique(c(unlist(cn)))
    L = lapply(L, function(x){
      x = ensure_colnames(x, cn)
      x[, cn]
    })
    L = do.call("rbind", L)
    return(L)
  }

  tag_content = bind_list(tag_content)
  cn = colnames(tag_content)
  cn[ cn == "name"] = "tag_name"
  colnames(tag_content) = cn

  url = paste0("https://api.github.com/repos/", repo, "/releases")

  res = httr::GET(url)

  ##########################
  # all releases
  ##########################
  hdrs = c("url", "assets_url", "upload_url", "html_url", "id", "tag_name",
           "target_commitish", "name", "draft", "prerelease",
           "created_at", "published_at", "tarball_url", "zipball_url")


  assets_hdrs = c("asset_updated_at", "asset_created_at",
                  "asset_name", "asset_label", "asset_download_count",
                  "asset_browser_download_url")
  cr = httr::content(res)
  df = lapply(cr, function(x) {
    dd = unlist_df(x[hdrs])
    dd = ensure_colnames(dd, hdrs)

    assets = bind_list(x$assets)
    if (!is.null(assets)) {
      colnames(assets) = paste0("asset_", colnames(assets))
      assets = ensure_colnames(assets, assets_hdrs)
      ret = merge(dd, assets, all = TRUE)
      return(ret)
    } else {
      return(NULL)
    }
  })
  df = do.call("rbind", df)

  if (is.null(df)) {
    return(NA)
  }
  if (nrow(df) == 0) {
    return(NA)
  }
  cn = c("asset_updated_at", "asset_created_at",
         "tag_name", "created_at", "published_at",
         "asset_name", "asset_label", "asset_download_count",
         "asset_browser_download_url")
  df = df[, cn]

  df = merge(tag_content, df, by = "tag_name", all.x = TRUE)

  make_time = function(times) {
    strptime(times, format = "%Y-%m-%dT%H:%M:%SZ")
  }
  df$created_at = make_time(df$created_at)
  df$published_at = make_time(df$published_at)
  df$asset_updated_at = make_time(df$asset_updated_at)
  df$asset_created_at = make_time(df$asset_created_at)


  ddf = df
  ddf = ddf[ grep(sys_ext(), ddf$asset_name, fixed = TRUE),]
  if (ref %in% ddf$commit.sha) {
    ddf = ddf[ ddf$commit.sha %in% ref, ]
  }
  ord = order(ddf$asset_created_at, decreasing = TRUE)
  ddf = ddf[ord, ]
  if (nrow(ddf) > 0) {
    ddf = ddf[1,]
    url = ddf$asset_browser_download_url
  } else {
    url = NA
  }
  return(url)
}

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

