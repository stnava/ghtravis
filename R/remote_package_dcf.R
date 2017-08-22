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
    res = vapply(remotes, get_remote_package_dcf, FUN.VALUE = NA_character_,
                 pat = pat, url = url, ...)
    names(res) = remotes
    res = vapply(res, unname, FUN.VALUE = NA_character_)
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
                   httr::write_disk(path = tmp),
                   httr::content_type("text/plain"))
  if (httr::status_code(req) >= 400) {
    tmp = NA_character_
  }
  names(tmp) = remotes
  return(tmp)
}

#' @export
#' @rdname get_remote_package_dcf
remote_package_dcf = function(...) {
  dcfs = get_remote_package_dcf(...)
  get_pack = function(tmp) {
    if (is.na(tmp)) {
      L = list(Package = NA,
               Version = NA)
    } else {
      L = read_dcf(tmp)$dcf
    }
    return(L)
  }
  if (length(dcfs) > 1) {
    L = lapply(dcfs, get_pack)
  } else {
    L = get_pack(dcfs)
  }

  return(L)
}


#' @export
#' @rdname get_remote_package_dcf
has_remote_dcf = function(
  remotes,
  pat = NULL,
  url = "https://github.com",
  ...) {
  if (is.null(pat)) {
    pat = devtools::github_pat(quiet = TRUE)
  }
  if (length(remotes) > 1) {
    res = vapply(remotes, has_remote_dcf, FUN.VALUE = logical(1),
                 pat = pat, url = url, ...)
    names(res) = remotes
    return(res)
  }
  remote = ghtravis::parse_one_remote(remotes)

  path <- paste(c(remote$username,
                  remote$repo, "raw", remote$ref,
                  remote$subdir, "DESCRIPTION"),
                collapse = "/")
  req <- httr::HEAD(url, path = path,
                    github_auth(pat),
                    httr::content_type("text/plain"))
  code = httr::status_code(req)
  code == 200
}