#' @title Get Remote Information
#' @description Parses DESCRIPTION file
#' @param path Path to DESCRIPTION file
#'
#' @return List of parsed files
#' @export
#'
#' @examples
#' path = example_description_file()
#' desc = get_remote_info(path)
get_remote_info = function(
  path = "DESCRIPTION"
  ) {

  remotes = get_remotes(path)
  remotes = split_remotes(remotes)
  if (length(remotes) == 0) {
    # remotes = ""
    return(list())
  }

  parsed = parse_remotes(remotes)
  names(parsed) = remotes
  # names(parsed) = sapply(parsed, `[[`, "repo")
  return(parsed)
}

#' @export
remote_package_names = function(path = "DESCRIPTION") {
  parsed = get_remote_info(path = path)
  n = sapply(parsed, `[[`, "repo")
  if (length(n) == 0) {
    return(NULL)
  }
  return(n)
}