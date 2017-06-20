#' Get Remote Package Dependencies
#'
#' @param path Path to DESCRIPTION file
#' @param dependencies List of dependencies to parse, passed to
#' \code{\link{get_dep_table}}
#' @param exclude_remotes exclude Remotes in dependency table
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
#' missing_remote_deps(path = path)
#' @export
all_remote_package_deps = function(
  path = "DESCRIPTION",
  ...,
  dependencies =  c("Depends", "Imports",
                    "LinkingTo", "Suggests"),
  exclude_remotes = TRUE) {
  repo = get_remotes(path = path)
  L = remote_package_deps(repo = repo,
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
  ...) {
  res = all_remote_package_deps(
    path = path,
    ...)
  res = res$name[ !(res$name %in% installed.packages()) ]
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
  }
  return(all(packs %in% installed.packages()))
}