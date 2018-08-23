
#' Order of the Remotes for Installation
#'
#' @param path Path to DESCRIPTION file
#' @param verbose Print diagnostic message
#' @param ... arguments passed to \code{\link{remote_package_deps}}
#' @param max_iter Number of iterations - shouldn't have to change,
#' stops from infinite loop
#'
#' @return Order of remotes
#' @export
#' @examples
#' path = example_description_file()
#' x = remote_order(path = path)
#' x
remote_order = function(
  path = "DESCRIPTION",
  ...,
  max_iter = 200){
  # @importFrom igraph graph_from_adjacency_matrix degree

  remotes = get_remotes(path = path)
  if (length(remotes) == 0) {
    return(NULL)
  }
  parsed = parse_remotes(remotes)
  all_packs = sapply(parsed, `[[`, "repo")
  # nulls = sapply(all_packs, is.null)
  # all_urls = sapply(parsed[nulls], `[[`, "url")


  # all_packs = sapply(parsed, function(x) {
  #   repo = x$repo
  #   if (!is.null(x$subdir)) {
  #     repo = x$subdir
  #   }
  #   return(repo)
  # })

  L = remote_package_deps(
    remotes = remotes,
    dependencies =  c("Depends", "Imports", "Suggests"),
    ...)
  packs = names(L)
  check = all_packs %in% packs
  if (!all(check)) {
    bad = all_packs[ !check ]
    bad = paste(bad, collapse = ", ")
    warning(paste0(
      "Remotes given for ", bad,
      " but not in Depends, Imports, Suggests.  If LinkingTo,",
      " then must be in Imports as well!"))
  }
  names(remotes) = packs
  dep_mat = sapply(L, function(x) {
    packs %in% x$name
    })
  dep_mat = as.matrix(dep_mat)
  rownames(dep_mat) = packs
  colnames(dep_mat) = names(L)

  install_order = list()
  i = 1
  while (length(packs) > 0) {
    # graph = igraph::graph_from_adjacency_matrix(
    #   dep_mat,
    #   mode = "directed")
    cs = colSums(dep_mat) == 0
    installer = names(cs)[cs]

    # outs = igraph::degree(graph, mode = "in")
    # installer = names(outs)[outs == 0]
    install_order = c(install_order,
                      list(installer))
    # no_dep = names(deg)[deg == 0]

    keep = !(packs %in% installer)
    dep_mat = dep_mat[keep, keep, drop = FALSE]
    packs = packs[keep]
    i = i + 1
    if (i > max_iter) {
      stop("something is wrong")
    }
  }
  install_order = unlist(install_order)
  remotes = remotes[ install_order]
  return(remotes)
}

#' @export
#' @rdname remote_order
reorder_remotes = function(
  path = "DESCRIPTION",
  verbose = TRUE,
  ...){
  remotes = remote_order(path = path, ...)
  if (verbose) {
    message(paste0("Remote order: ", paste(remotes, collapse = ", ")))
  }
  res = rewrite_remotes(path = path, remotes = remotes)
  return(res)
}




