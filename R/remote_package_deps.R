#' Get Remote Package Dependencies
#'
#' @param path Path to DESCRIPTION file
#' @param remotes Repository if one specific
#' @param dependencies List of dependencies to parse, passed to
#' \code{\link{get_dep_table}}
#' @param exclude_remotes exclude Remotes in dependency table
#' @param verbose Print diagnostic message
#' @param ... arguments passed to \code{\link{get_remote_package_dcf}}
#'
#' @return A \code{data.frame} of Dependencies
#' (\code{all_remote_package_deps}) or a list of a
#' package dependency table and a vector of package names for the
#' remotes (\code{remote_package_deps})
#' @export
#'
#' @examples
#' remote_package_deps("stnava/ANTsRCore")
#' path = example_description_file()
#' all_remote_package_deps(path = path)
#' all_remote_package_deps(remotes = c("stnava/ANTsRCore", "stnava/ITKR"))
#' missing_remote_deps(path = path)
#' @export
all_remote_package_deps = function(
  path = "DESCRIPTION",
  remotes = NULL,
  verbose = TRUE,
  ...,
  dependencies =  c("Depends", "Imports",
                    "LinkingTo", "Suggests"),
  exclude_remotes = TRUE) {
  remotes = null_if_empty(remotes)

  if (is.null(remotes)) {
    remotes = get_remotes(path = path)
  } else {
    if (verbose) {
      message("Repositories are given, overriding path")
    }
  }
  L = remote_package_deps(
    remotes = remotes,
    dependencies = dependencies,
    ...)
  packs = names(L)
  deps = do.call("rbind", L)
  rownames(deps) = NULL
  deps = unique(deps)
  if (exclude_remotes) {
    deps = deps[ !(deps$name %in% packs), , drop = FALSE]
  }

  return(deps)
}

#' @rdname all_remote_package_deps
#' @export
remote_package_deps = function(
  ...,
  dependencies =  c("Depends", "Imports",
                    "LinkingTo", "Suggests")) {
  tmp_dcf = get_remote_package_dcf(...)
  L = vector(mode = "list", length = length(tmp_dcf))
  # packs = rep(NA, length = length(tmp_dcf))
  for (iL in seq_along(tmp_dcf)) {
    tmp = tmp_dcf[[iL]]
    if (is.na(tmp)) {
      L[[iL]] = NULL

    } else {
      L[[iL]] = get_dep_table(path = tmp,
                              dependencies = dependencies)
      names(L)[iL] = read_dcf(tmp)$dcf$Package
      # packs[iL] = read_dcf(tmp)$dcf$Package
    }
  }
  # names(L) = packs
  return(L)
}

#' @rdname all_remote_package_deps
#' @export
missing_remote_deps = function(
  path = "DESCRIPTION",
  verbose = TRUE,
  ...) {
  res = all_remote_package_deps(
    path = path,
    verbose = verbose,
    ...)
  res$installed = res$name %in% installed.packages()
  if (verbose) {
    print(res[, c("name", "installed")])
    message("All installed dependencies are ",
            paste(res$name[res$installed], collapse = ","))
    message("All missing dependencies are ",
            paste(res$name[!res$installed], collapse = ","))
  }
  res = res$name[ !res$installed ]
  return(res)
}


#' @rdname all_remote_package_deps
#' @export
install_missing_remote_deps = function(
  path = "DESCRIPTION",
  ...) {
  packs = missing_remote_deps(path = path, ...)
  if (length(packs) > 0) {
    install.packages(packs)
    return(all(packs %in% installed.packages()))
  } else {
    return(TRUE)
  }
}