#' Print Description Elements out
#'
#' @param path path to DESCRIPTION file
#' @param repo remote repository on GitHub
#' @param ... Additional arguments passed to \code{\link{get_remote_package_dcf}}
#'
#' @return Nothing
#' @export
#' @import desc
#' @importFrom utils as.person

cat_desc_elements = function(path = "DESCRIPTION"){
  desc = description$new(file = path)
  desc$coerce_authors_at_r()
  desc$del("Author")
  pack = unname(desc$get("Package"))
  man = desc$get_maintainer()
  man = as.person(man)

  ver = as.character(desc$get_version())
  dep_types = c("Depends",
                "Imports",
                "Suggests")
  deps = desc$get_deps()
  deps = deps[ deps$type %in% dep_types,, drop = FALSE]

  collapser = function(get_type) {
    x = deps[ deps$type %in% get_type,, drop = FALSE]
    if (nrow(x) > 0) {
      pack = x$package
      pack = setdiff(pack, "R")
      # pack = setdiff(pack,
      # 	c("R", "methods",
      # 		"stats", "utils",
      # 		"base",
      # 		"graphics",
      # 		"grDevices"))
      pack = paste(pack,
                   collapse = ", ")
    } else {
      pack = NULL
    }
    return(pack)
  }
  col_deps = lapply(dep_types,
                    collapser)
  names(col_deps) = dep_types

  paster = function(x, y) {
    if (length(y) == 0) {
      return(NULL)
    }
    paste(x, y)
  }

  res = c(
    paste("Name:", pack),
    paste("Version:", ver),
    paste("Maintainer:", format(man,
                                include = c("given", "family"))),
    paste("Email:", man$email),
    paster("Depends:", col_deps[["Depends"]]),
    paster("Imports:", col_deps[["Imports"]]),
    paster("Suggests:", col_deps[["Suggests"]])
  )
  cat(res, sep = "\n")
  return(invisible(NULL))
}

#' @export
#' @rdname cat_desc_elements
cat_desc_elements_remote = function(repo, ...) {
  path = get_remote_package_dcf(remotes = repo, ...)
  cat_desc_elements(path = path)
}