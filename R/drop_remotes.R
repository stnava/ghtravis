#' @title Drop Remotes from DESCRIPTION
#' @description Reads a DESCRIPTION file, drops the remotes, and then rewrites
#' the DESCRIPTION file
#'
#' @param path Path to DESCRIPTION file
#' @param drop_remotes Packages for Remotes to Drop
#' @param reorder Should remotes be reordered after running
#' @param ... if \code{reorder = TRUE} then arguments passed to
#' \code{\link{reorder_remotes}}
#'
#' @return Rewrites the DESCRIPTION file without remotes
#' @export
#'
#' @examples
#' path = example_description_file()
#' x = drop_remotes(path, reorder = TRUE)
#' readLines(x)
drop_remotes = function(
  path = "DESCRIPTION",
  drop_remotes = NULL,
  reorder = FALSE,
  ...) {

  # remotes = get_remotes(path )
  rres = read_dcf(path)
  res = rres$dcf
  # nres = names(res)
  remotes = get_remotes(res)
  if (length(remotes) == 0) {
    return(path)
  }

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
    if (length(remotes) == 0 ) {
      remotes = NULL
    }
    if (all(remotes == "")) {
      remotes = NULL
    }
  } else {
    remotes = NULL
  }

  res = rewrite_remotes(
    path = path,
    remotes = remotes)
  if (reorder) {
    res = reorder_remotes(path = path, ...)
  }
  return(res)
}


