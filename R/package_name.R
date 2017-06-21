#' Get Package name
#'
#' @param path Path to DESCRIPTION file
#'
#' @return Name of Packge
#' @export
#'
#' @examples
#' path = example_description_file()
#' package_name(path)
package_name = function(path = "DESCRIPTION") {
  dcf = read_dcf(path = path)$dcf
  dcf$Package
}