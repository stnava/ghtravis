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
  dcf = read.dcf(file,
                 keep.white = fields, all = TRUE)
  dcf = dcf_collapser(dcf, cn = c("Imports", "Suggests", "Depends"))
  dcf = as.list(dcf[1, ])
  return(list(fields = fields,
              dcf = dcf))
}