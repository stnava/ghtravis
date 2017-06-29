#' @title Get Comparison Table from Dependencies
#' @description Gets Remotes and compares to installed packages
#'
#' @param path Path to DESCRIPTION file
#' passed to \code{\link{remote_binaries}}
#' @param dependencies List of dependencies to parse
#' @param ... not used
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' path = example_description_file()
#' update_these_packages(path = path)
package_comparison_table = function(
  path = "DESCRIPTION",
  dependencies = c("Depends", "Imports",
                   "LinkingTo")
) {

  dd = get_dep_table( path = path,
                      dependencies = dependencies,
                      limit_compare = FALSE)
  cn = colnames(dd)
  cn[ cn == "name"] = "Package"
  cn[ cn == "version"] = "required_version"
  colnames(dd) = cn
  ip = installed.packages()[, c("Package", "Version")]
  ip = data.frame(ip, stringsAsFactors = FALSE)

  dd = merge(dd, ip, all.x = TRUE)
  # workaround
  dd$required_version[ is.na(dd$required_version)] = "0.0.0.0"
  dd$required_version[ is.na(dd$Version) ] = "9999.0.0.0"
  dd$Version[ is.na(dd$Version)] = "0.0.0.0"

  dd$comp = apply(dd[, c("required_version", "Version")], 1, function(x) {
    x = make_full_version(x)
    compareVersion(x[1], x[2])
  })
  return(dd)
}


#' @rdname package_comparison_table
#' @export
update_these_packages = function(
 ...
) {
  dd = package_comparison_table(...)
  keep_packs = dd[ dd$comp > 0 |
                     (dd$compare %in% ">" & dd$comp <= 0) , "Package"]
  # removers = keep_packs[ keep_packs %in% ip ]
  return(keep_packs)
}