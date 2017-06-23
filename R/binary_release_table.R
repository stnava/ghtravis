#' @title Get Binary Release Table for a repository
#' @description Parses a remote and gets a table of binary releases
#'
#' @param repo Remote repository name
#' @param pat GitHub Personal Authentication Token (PAT)
#' @param ... additional arguments to \code{\link[httr]{GET}}
#' @return \code{data.frame} of binary releases
#' @export
#'
#' @examples
#' repo = "stnava/ANTsR"
#' binary_release_table(repo)
#' binary_release_table("muschellij2/ANTsR")
#' @importFrom httr GET content stop_for_status authenticate
#' @importFrom devtools github_pat
binary_release_table = function(
  repo,
  pat = NULL,
  ...){

  xrepo = repo
  if (is.null(pat)) {
    pat = devtools::github_pat(quiet = TRUE)
  }
  info = parse_one_remote(repo)
  user = info$username
  package = info$repo
  # ref = info$ref
  repo = paste0(user, "/", package)

  tag_content = tag_table(repo = xrepo, pat = pat, ...)

  nt = nrow(tag_content)
  if (is.null(nt)) {
    nt = 0
  }
  if (nt == 0) {
    cn = c("tag_name", "zipball_url", "tarball_url", "commit.sha", "commit.url")
    tag_content = t(rep(NA, length = length(cn)))
    colnames(tag_content) = cn
    tag_content = as.data.frame(tag_content, stringsAsFactors = FALSE)    
    # return(NA) # there should be no releases, but there may be somehow?
  }

  df = binary_table_no_tags(repo = xrepo, pat = pat, ...)
  if (all(is.na(df))) {
    return(NA)
  }

  cn = c("asset_updated_at", "asset_created_at",
         "tag_name", "created_at", "published_at",
         "asset_name", "asset_label", "asset_download_count",
         "asset_browser_download_url")
  df = df[, cn]

  # df = merge(tag_content, df, by = "tag_name", all.x = TRUE)
  df = merge(tag_content, df, by = "tag_name", all = TRUE)
  
  make_time = function(times) {
    strptime(times, format = "%Y-%m-%dT%H:%M:%SZ")
  }
  df$created_at = make_time(df$created_at)
  df$published_at = make_time(df$published_at)
  df$asset_updated_at = make_time(df$asset_updated_at)
  df$asset_created_at = make_time(df$asset_created_at)

  ord = order(df$asset_created_at, df$asset_updated_at, decreasing = TRUE)
  df = df[ord, ]

  return(df)
}

#' @rdname binary_release_table
#' @export
#' @examples
#' repo = "stnava/ANTsR"
#' binary_table_no_tags(repo)
#' binary_table_no_tags("muschellij2/ANTsR")
binary_table_no_tags = function(
  repo,
  pat = NULL,
  ...){

  info = parse_one_remote(repo)
  user = info$username
  package = info$repo
  # ref = info$ref
  repo = paste0(user, "/", package)

  if (is.null(pat)) {
    pat = devtools::github_pat(quiet = TRUE)
  }
  url = paste0("https://api.github.com/repos/", repo, "/releases")

  res = httr::GET(url, github_auth(pat), ...)
  httr::stop_for_status(res)

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
  if (length(cr) == 0 ) {
    return(NA)
  }
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
  return(df)
}
