#' @title System Extension
#' @description Gets System Extension for tarball
#'
#' @return Extension for the operating system
#' @export
#'
#' @examples
#' sys_ext()
sys_ext = function(){
  os = Sys.info()
  os = os[["sysname"]]
  ext = switch(
    os,
    Windows = ".zip$",
    Linux = "_R_x86_64-pc-linux-gnu.*.tar.gz$",
    Darwin = ".tgz$"
  )
  return(ext)
}