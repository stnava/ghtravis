#' @title Rewrite Travis for Deployment
#' @description Writes the Deployment part of the Travis YAML for a binary
#' package
#'
#' @param api_key API Key to Travis Key, Secure
#' @param path Path to DESCRIPTION file
#' @param travis_file Travis File
#' @param user GitHub username of repository
#'
#' @return Name of travis file
#' @export
#'
#' @importFrom yaml yaml.load as.yaml
#' @examples
#' travis_file = example_travis_file()
#' path = example_description_file()
#' outfile = deploy_travis(api_key = "test",
#' path = path,
#' travis_file = travis_file)
#' readLines(outfile)
#'
deploy_travis = function(api_key,
                         path = "DESCRIPTION",
                         travis_file = ".travis.yml",
                         user = "neuroconductor"
) {

  res = read_dcf(path)
  package = res$dcf$Package
  if (!file.exists(travis_file)) {
    stop("TRAVIS FILE NOT FOUND!")
  }
  trav = readLines(travis_file)
  trav = paste(trav, collapse = "\n")
  trav = yaml::yaml.load(string = trav)
  deploy = list(deploy = list(
    provider = "releases",
    skip_cleanup = TRUE,
    api_key = list(
      secure = api_key),
    file_glob = TRUE,
    file = paste0(package, "*.t*gz"),
    on = list(
      tags = TRUE, repo = paste0(user, "/", package)
    )
  ))
  trav$deploy = NULL
  trav = yaml::as.yaml(trav,
                       indent.mapping.sequence = TRUE)
  deploy = yaml::as.yaml(deploy)
  deploy = gsub("\n'on':\n", "\non:\n",
                deploy, fixed = TRUE)
  trav = paste(trav, deploy, sep = "\n")
  writeLines(trav, con = travis_file)
  return(travis_file)
}