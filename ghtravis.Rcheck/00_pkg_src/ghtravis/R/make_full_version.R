#' @title Make a vector of version numbers into full versions for comparison
#' @description The \code{compareVersion} function requires full version
#' numbers for sufficient comparion
#'
#' @param x character vector of version numbers
#'
#' @return Character vector of full version numbers
#' @export
#'
#' @examples
#' x = c("0.15", "0.15.0")
#' compareVersion(x[1], x[2])
#' full = make_full_version(x)
#' compareVersion(full[1], full[2])
make_full_version = function(x) {
  nx = names(x)
  x = as.character(x)
  r <- lapply(strsplit(x, "[.-]"),
              as.integer)
  lx = sapply(r, length)
  mlx = max(lx)
  r <- lapply(r, function(ver) {
    c(ver, rep(0,
               length = mlx - length(ver)))
  })
  x = sapply(r, paste, collapse = ".")
  names(x) = nx
  return(x)
}