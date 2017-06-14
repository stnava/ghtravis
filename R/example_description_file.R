#' @title Example DESCRIPTION File
#' @description Copies example DESCRIPTION file for parsing
#'
#' @return Path of DESCRIPTION file
#' @export
#'
#' @examples
#' example_description_file()
example_description_file = function() {
  x = system.file("extdata", "DESCRIPTION", package = "ghtravis")
  if (x == "") {
    return("")
    }
  tfile = tempfile()
  dir.create(tfile)
  file.copy(x, to = tfile, overwrite = TRUE)
  x = file.path(tfile, "DESCRIPTION")
  return(x)
}

#' @rdname example_description_file
#' @export
example_travis_file = function() {
  x = system.file("extdata", "travis.yml", package = "ghtravis")
  if (x == "") {
    return("")
  }
  tfile = tempfile()
  dir.create(tfile)
  file.copy(x, to = tfile, overwrite = TRUE)
  x = file.path(tfile, "travis.yml")
  return(x)
}