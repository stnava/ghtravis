
#' Rewrite DESCRIPTION file overwriting Remotes
#'
#' @param path Path to DESCRIPTION file
#' @param remotes Packages for remotes field
#'
#' @return Name of path
#' @export
rewrite_remotes = function(path, remotes = NULL) {
  rres = read_dcf(path = path)
  res = rres$dcf
  if (!is.null(remotes)) {
    if (!all(remotes == "")) {
      remotes = remotes[ remotes != "" ]
      remotes = paste(remotes, collapse = ", ")
    } else {
      remotes = NULL
    }
  }
  res$Remotes = remotes
  nres = names(res)
  res = as.data.frame(res,
                      stringsAsFactors = FALSE)
  names(res) = nres

  write.dcf(x = res,
            file = path)
  return(path)
}