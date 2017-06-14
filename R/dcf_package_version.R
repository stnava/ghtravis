#' @title Get the version of the package from the DESCRIPTION file
#' @description Extracts Package version from DESCRIPTION file
#'
#' @param path Path to DESCRIPTION file
#'
#' @return Return the Package Version
#' @export
#'
#' @examples
#' path = example_description_file()
#' desc = dcf_package_version(path)
dcf_package_version = function(path = "DESCRIPTION"){
  res = read_dcf(path)
  res$dcf$Version
}