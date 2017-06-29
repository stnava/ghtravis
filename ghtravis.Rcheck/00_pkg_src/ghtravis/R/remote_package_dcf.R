#' Read Remote Package DESCRIPTION file
#'
#' @param remotes Repository name
#' @param pat GitHub Personal Authentication Token (PAT)
#' @param url URL for repo - github
#' @param ... not used
#'
#' @return List of Package/Version DCF information
#' @export
#'
#' @examples
#' get_remote_package_dcf("stnava/ANTsRCore")
#' remote_package_dcf("stnava/ANTsRCore")
get_remote_package_dcf = function(
  remotes,
  pat = NULL,
  url = "https://github.com",
  ...) {
  if (is.null(pat)) {
    pat = devtools::github_pat(quiet = TRUE)
  }

  if (length(remotes) > 1) {
    res = lapply(remotes, get_remote_package_dcf, pat = pat, url = url, ...)
    return(res)
  }
  remote = parse_one_remote(remotes)

  tmp <- tempfile()
  path <- paste(c(remote$username,
                  remote$repo, "raw", remote$ref,
                  remote$subdir, "DESCRIPTION"),
                collapse = "/")
  req <- httr::GET(url, path = path,
                   github_auth(pat),
                   httr::write_disk(path = tmp))
  if (httr::status_code(req) >= 400) {
    tmp = NA
  }
  return(tmp)
}

#' @export
#' @rdname get_remote_package_dcf
remote_package_dcf = function(...) {
  tmp = get_remote_package_dcf(...)
  if (is.na(tmp)) {
    L = list(Package = NA,
             Version = NA)
  } else {
    L = read_dcf(tmp)$dcf
  }
  return(L)
}


