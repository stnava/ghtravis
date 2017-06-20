
#' Order of the Remotes for Installation
#'
#' @param path Path to DESCRIPTION file
#' @param ... arguments passed to \code{\link{remote_package_deps}}
#' @param max_iter Number of iterations - shouldn't have to change,
#' stops from infinite loop
#'
#' @return Order of remotes
#' @export
#' @importFrom igraph graph_from_adjacency_matrix degree
#' @examples
#' path = example_description_file()
#' x = remote_order(path = path)
#' x
remote_order = function(
  path = "DESCRIPTION",
  ...,
  max_iter = 200){

  repo = get_remotes(path = path)
  if (length(repo) == 0) {
    return(NULL)
  }
  parsed = parse_remotes(repo)
  all_packs = sapply(parsed, `[[`, "repo")

  L = remote_package_deps(
    repo = repo,
    dependencies =  c("Depends", "Imports"),
    ...)
  packs = names(L)
  stopifnot(all(all_packs %in% packs))
  names(repo) = packs
  dep_mat = sapply(L, function(x) {
    packs %in% x$name
    })
  rownames(dep_mat) = packs

  install_order = list()
  i = 1
  while(length(packs) > 0) {
    graph = igraph::graph_from_adjacency_matrix(
      dep_mat,
      mode = "directed")

    outs = igraph::degree(graph, mode = "in")
    installer = names(outs)[outs == 0]
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
  remotes = repo[ install_order]
  return(remotes)
}

#' @export
#' @rdname remote_order
reorder_remotes = function(
  path = "DESCRIPTION",
  ...){
  remotes = remote_order(path = path, ...)
  res = rewrite_remotes(path = path, remotes = remotes)
  return(res)
}




