#' @title Get Dependency Table
#' @description Get a Table of the Dependencies
#'
#' @param path Path to DESCRIPTION file
#' @param dependencies List of dependencies to parse
#' @param limit_compare only keep rows where a comparison operator is found
#'
#' @return A \code{data.frame} of
#' \describe{
#' \item{name}{name of the package}
#' \item{compare}{comparison operator}
#' \item{version}{version of the package}
#' }
#' @export
#'
#' @examples
#' path = example_description_file()
#' get_dep_table(path = path)
get_dep_table = function(
  path = "DESCRIPTION",
  dependencies =  c("Depends", "Imports",
                    "LinkingTo", "Suggests"),
  limit_compare = FALSE) {
  pack = read_dcf(path = path)$dcf
  dependencies = pack[dependencies]
  parsed <- lapply(dependencies,
                   devtools::parse_deps)
  deps = do.call("rbind", parsed)
  if (nrow(deps) == 0) {
    return(NULL)
  }
  rownames(deps) = NULL
  if (limit_compare) {
    deps = deps[ !is.na(deps$compare), , drop = FALSE]
  }
  deps = unique(deps)
  return(deps)
}