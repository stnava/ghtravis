null_if_empty = function(x) {
  if (length(x) == 0 ) {
    x = NULL
  }
  if (!is.null(x)) {
    if (all(x == "")) {
      x = NULL
    }
  }
  return(x)
}

github_auth <- function(token) {
  if (is.null(token)) {
    NULL
  } else {
    httr::authenticate(token, "x-oauth-basic", "basic")
  }
}

unlist_df = function(x) {
  x = unlist(x)
  x = as.data.frame(t(x), stringsAsFactors = FALSE)
}
ensure_colnames = function(x, cn) {
  sd = setdiff(colnames(x), cn)
  for (isd in sd) {
    x[, isd] = NA
  }
  sd = setdiff(cn, colnames(x))
  for (isd in sd) {
    x[, isd] = NA
  }
  return(x)
}
bind_list = function(L) {
  L = lapply(L, unlist_df)
  cn = sapply(L, colnames)
  cn = unique(c(unlist(cn)))
  L = lapply(L, function(x){
    x = ensure_colnames(x, cn)
    x[, cn]
  })
  L = do.call("rbind", L)
  return(L)
}




get_next = function(links, ind = "next") {
  if (inherits(links, "response")) {
    links = links$headers$link
  }
  if (is.null(links)) {
    return(NULL)
  }
  links <- trimws(strsplit(links, ",")[[1]])
  link_list <- lapply(links, function(x) {
    x <- trimws(strsplit(x, ";")[[1]])
    name <- sub("^.*\"(.*)\".*$", "\\1", x[2])
    value <- sub("^<(.*)>$", "\\1", x[1])
    c(name, value)
  })
  link_list <- structure(vapply(link_list, "[", "", 2),
                         names = vapply(link_list,
                                        "[", "", 1))
  if (ind %in% names(link_list)) {
    link_list[[ind]]
  }
  else {
    NULL
  }
}