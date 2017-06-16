#' @title Tag Table
#' @description Parses a remote and gets a table of tags
#'
#' @param repo Remote repository name
#' @param pat GitHub Personal Authentication Token (PAT)
#' @param ... additional arguments to \code{\link[httr]{GET}}
#' @return \code{data.frame} of tags
#' @export
#'
#' @examples
#' repo = "stnava/ANTsR"
#' tag_table(repo)
#' tag_table("muschellij2/ANTsR")
#' @importFrom httr GET content stop_for_status authenticate
#' @importFrom devtools github_pat
tag_table = function(repo, pat = NULL, ...) {
  if (is.null(pat)) {
    pat = devtools::github_pat(quiet = TRUE)
  }
  tag_url = paste0("https://api.github.com/repos/", repo, "/tags")
  # args = list(url = tag_url)
  tag_res = httr::GET(tag_url, github_auth(pat), ...)
  httr::stop_for_status(tag_res)
  tag_content = httr::content(tag_res)


  tag_content = bind_list(tag_content)
  if (!is.null(tag_content)) {
    if (ncol(tag_content) > 0) {
      cn = colnames(tag_content)
      cn[ cn == "name"] = "tag_name"
      colnames(tag_content) = cn
    }
  }
  return(tag_content)
}