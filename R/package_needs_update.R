#' @title Check Package Versions
#' @description Looks in installed packages to see if a new version is needed
#'
#' @param version Version of package
#' @param package Package to check
#' @param ... not used
#' @return Output from \code{\link{compareVersion}}
#' @export
#'
#' @importFrom utils compareVersion
package_needs_update = function(
  version,
  package) {
  ip = installed.packages()
  if (!(package %in% ip)) {
    return(TRUE)
  }
  cur_ver = ip[ package, "Version"]
  v = c("version" = version,
        "installed" = cur_ver)
  v = make_full_version(v)
  compareVersion(v["version"],
                 v["installed"]) > 0
}

#' @export
set_update_var = function(...) {
  res = package_needs_update(...)
  return(as.numeric(res))
}
