#' @title Read DESCRIPTION file
#' @description Reads a DESCRIPTION file
#'
#' @param path path to the DESCRIPTION file
#' @return List of the fields and the data.frame of the description information
#' @export
#'
read_dcf <- function(path = "DESCRIPTION") {
  file = file(path)
  on.exit({
    close(file)
  })
  fields <- colnames(read.dcf(file))
  dcf = as.list(read.dcf(file,
                         keep.white = fields, all = TRUE)[1, ])
  return(list(fields = fields,
              dcf = dcf))
}