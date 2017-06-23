#' @title Get the Latest Binary for a repository
#' @description Parses a remote and gets a URL for a binary release
#'
#' @param repo Remote repository name
#' @param pat GitHub Personal Authentication Token (PAT)
#' @param verbose print diagnostic messages
#' @param ... additional arguments to \code{\link[httr]{GET}}
#' @return URL of the binary
#' @export
#'
#' @examples
#' repo = "stnava/ANTsR"
#' latest_release_with_binary(repo)
#' @importFrom httr GET content stop_for_status authenticate
#' @importFrom devtools github_pat
latest_release_with_binary = function(repo,
                                      pat = NULL,
                                      verbose = TRUE,
                                      ...){

  df = binary_release_table(repo = repo, pat = pat, verbose = verbose, ...)
  if (is.null(nrow(df))) {
    return(NA)
  }
  if (is.vector(df)) {
    if (all(is.na(df))) {
      return(NA)
    }
  }
  info = parse_one_remote(repo)
  user = info$username
  package = info$repo
  ref = info$ref
  repo = paste0(user, "/", package)


  ddf = df
  ddf = ddf[ grep(sys_ext(), ddf$asset_name, fixed = TRUE),]
  if (!(ref %in% "master")) {
    if (!(ref %in% ddf$commit.sha)) {
      warning(paste0("SHA was given, but no release associated",
                     " with it!, not installing, returning NA"))
      return(NA)
    } else {
      ddf = ddf[ ddf$commit.sha %in% ref, ]
    }
  }
  ord = order(ddf$asset_created_at, ddf$asset_updated_at, decreasing = TRUE)
  ddf = ddf[ord, ]
  if (verbose) {
    message("Table is")
    print(ddf)
  }
  if (nrow(ddf) > 0) {
    ddf = ddf[1,]
    url = ddf$asset_browser_download_url
  } else {
    url = NA
  }
  return(url)
}
