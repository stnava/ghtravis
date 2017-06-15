#' @title Drop Remotes from DESCRIPTION
#' @description Reads a DESCRIPTION file, drops the remotes, and then rewrites
#' the DESCRIPTION file
#'
#' @param path Path to DESCRIPTION file
#' @param drop_remotes Packages for Remotes to Drop
#'
#' @return Rewrites the DESCRIPTION file without remotes
#' @export
#'
#' @examples
#' path = example_description_file()
#' x = drop_remotes(path)
#' readLines(x)
drop_remotes = function(
  path = "DESCRIPTION",
  drop_remotes = NULL) {

  # remotes = get_remotes(path )
  rres = read_dcf(path)
  res = rres$dcf
  # nres = names(res)
  remotes = get_remotes(res)

  parsed = parse_remotes(remotes)
  pack_with_remote = sapply(parsed, function(x) {
    x$repo
  })
  # in case passing in remotes themselves
  if (any(grepl("/", drop_remotes))) {
    drop_remotes = parse_remotes(drop_remotes)
    drop_remotes = sapply(drop_remotes, `[[`, "repo")
  }
  if (!is.null(drop_remotes)) {
    remotes = remotes[!(pack_with_remote %in% drop_remotes)]
    remotes = paste(remotes, collapse = ", ")
  } else {
    remotes = NULL
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


