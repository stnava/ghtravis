#' Get R Version
#'
#' @return A character string with just major and
#' minor (no patch or hotfix) version of R
#'
#' @export
#'
#' @examples
#' r_version()
r_version = function() {
  ver = R.Version()
  major = ver$major
  minor = ver$minor
  minor = strsplit(minor, split = "[.]")[[1]][1]
  paste0(major, ".", minor)
}