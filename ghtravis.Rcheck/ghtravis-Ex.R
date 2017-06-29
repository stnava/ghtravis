pkgname <- "ghtravis"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ghtravis')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("all_remote_package_deps")
### * all_remote_package_deps

flush(stderr()); flush(stdout())

### Name: all_remote_package_deps
### Title: Get Remote Package Dependencies
### Aliases: all_remote_package_deps remote_package_deps
###   missing_remote_deps install_missing_remote_deps

### ** Examples

remote_package_deps("stnava/ANTsRCore")
path = example_description_file()
all_remote_package_deps(path = path)
all_remote_package_deps(remotes = c("stnava/ANTsRCore", "stnava/ITKR"))
missing_remote_deps(path = path)



cleanEx()
nameEx("binary_release_table")
### * binary_release_table

flush(stderr()); flush(stdout())

### Name: binary_release_table
### Title: Get Binary Release Table for a repository
### Aliases: binary_release_table binary_table_no_tags

### ** Examples

repo = "stnava/ANTsR"
binary_release_table(repo)
binary_release_table("muschellij2/ANTsR")
repo = "stnava/ANTsR"
binary_table_no_tags(repo)
binary_table_no_tags("muschellij2/ANTsR")



cleanEx()
nameEx("dcf_package_version")
### * dcf_package_version

flush(stderr()); flush(stdout())

### Name: dcf_package_version
### Title: Get the version of the package from the DESCRIPTION file
### Aliases: dcf_package_version

### ** Examples

path = example_description_file()
desc = dcf_package_version(path)



cleanEx()
nameEx("deploy_travis")
### * deploy_travis

flush(stderr()); flush(stdout())

### Name: deploy_travis
### Title: Rewrite Travis for Deployment
### Aliases: deploy_travis

### ** Examples

travis_file = example_travis_file()
path = example_description_file()
outfile = deploy_travis(api_key = "test",
path = path,
travis_file = travis_file)
readLines(outfile)




cleanEx()
nameEx("deployed_tarball_version")
### * deployed_tarball_version

flush(stderr()); flush(stdout())

### Name: deployed_tarball_version
### Title: Deployed Tarball Version
### Aliases: deployed_tarball_version deployed_tarball

### ** Examples

deployed_tarball_version(  repo = "stnava/ITKR", tag = "latest")



cleanEx()
nameEx("drop_remotes")
### * drop_remotes

flush(stderr()); flush(stdout())

### Name: drop_remotes
### Title: Drop Remotes from DESCRIPTION
### Aliases: drop_remotes

### ** Examples

path = example_description_file()
x = drop_remotes(path, reorder = TRUE)
readLines(x)



cleanEx()
nameEx("example_description_file")
### * example_description_file

flush(stderr()); flush(stdout())

### Name: example_description_file
### Title: Example DESCRIPTION File
### Aliases: example_description_file example_travis_file

### ** Examples

example_description_file()



cleanEx()
nameEx("get_dep_table")
### * get_dep_table

flush(stderr()); flush(stdout())

### Name: get_dep_table
### Title: Get Dependency Table
### Aliases: get_dep_table

### ** Examples

path = example_description_file()
get_dep_table(path = path)



cleanEx()
nameEx("get_remote_info")
### * get_remote_info

flush(stderr()); flush(stdout())

### Name: get_remote_info
### Title: Get Remote Information
### Aliases: get_remote_info remote_package_names

### ** Examples

path = example_description_file()
desc = get_remote_info(path)



cleanEx()
nameEx("get_remote_package_dcf")
### * get_remote_package_dcf

flush(stderr()); flush(stdout())

### Name: get_remote_package_dcf
### Title: Read Remote Package DESCRIPTION file
### Aliases: get_remote_package_dcf remote_package_dcf

### ** Examples

get_remote_package_dcf("stnava/ANTsRCore")
remote_package_dcf("stnava/ANTsRCore")



cleanEx()
nameEx("get_remotes")
### * get_remotes

flush(stderr()); flush(stdout())

### Name: get_remotes
### Title: Get the Remotes from a Package
### Aliases: get_remotes

### ** Examples

path = example_description_file()
get_remotes(path)



cleanEx()
nameEx("install_remote_binaries")
### * install_remote_binaries

flush(stderr()); flush(stdout())

### Name: install_remote_binaries
### Title: Install Binaries from Remotes
### Aliases: install_remote_binaries

### ** Examples

## Not run: 
##D path = example_description_file()
##D install_remote_binaries(path = path)
## End(Not run)



cleanEx()
nameEx("install_remotes_no_dep")
### * install_remotes_no_dep

flush(stderr()); flush(stdout())

### Name: install_remotes_no_dep
### Title: Returns the specific package from Remotes
### Aliases: install_remotes_no_dep

### ** Examples

## Not run: 
##D   install_remotes_no_dep()
## End(Not run)



cleanEx()
nameEx("latest_release_with_binary")
### * latest_release_with_binary

flush(stderr()); flush(stdout())

### Name: latest_release_with_binary
### Title: Get the Latest Binary for a repository
### Aliases: latest_release_with_binary

### ** Examples

repo = "stnava/ANTsR"
latest_release_with_binary(repo)



cleanEx()
nameEx("make_full_version")
### * make_full_version

flush(stderr()); flush(stdout())

### Name: make_full_version
### Title: Make a vector of version numbers into full versions for
###   comparison
### Aliases: make_full_version

### ** Examples

x = c("0.15", "0.15.0")
compareVersion(x[1], x[2])
full = make_full_version(x)
compareVersion(full[1], full[2])



cleanEx()
nameEx("package_comparison_table")
### * package_comparison_table

flush(stderr()); flush(stdout())

### Name: package_comparison_table
### Title: Get Comparison Table from Dependencies
### Aliases: package_comparison_table update_these_packages

### ** Examples

path = example_description_file()
update_these_packages(path = path)



cleanEx()
nameEx("package_name")
### * package_name

flush(stderr()); flush(stdout())

### Name: package_name
### Title: Get Package name
### Aliases: package_name

### ** Examples

path = example_description_file()
package_name(path)



cleanEx()
nameEx("parse_one_remote")
### * parse_one_remote

flush(stderr()); flush(stdout())

### Name: parse_one_remote
### Title: Parse a Remote file
### Aliases: parse_one_remote parse_remotes

### ** Examples

parse_one_remote("stnava/ANTsR")



cleanEx()
nameEx("remote_binaries")
### * remote_binaries

flush(stderr()); flush(stdout())

### Name: remote_binaries
### Title: Get Binaries from Remotes
### Aliases: remote_binaries

### ** Examples

remote_binaries(remotes = "muschellij2/neurobase")
path = example_description_file()
if (file.exists(path)) {
   remote_binaries(path = path)
}



cleanEx()
nameEx("remote_order")
### * remote_order

flush(stderr()); flush(stdout())

### Name: remote_order
### Title: Order of the Remotes for Installation
### Aliases: remote_order reorder_remotes

### ** Examples

path = example_description_file()
x = remote_order(path = path)
x



cleanEx()
nameEx("split_remotes")
### * split_remotes

flush(stderr()); flush(stdout())

### Name: split_remotes
### Title: Remotes Splitter
### Aliases: split_remotes

### ** Examples

split_remotes(c("stnava/ANTsR, blah/package@asdf"))



cleanEx()
nameEx("subset_remote")
### * subset_remote

flush(stderr()); flush(stdout())

### Name: subset_remote
### Title: Returns the specific package from Remotes
### Aliases: subset_remote

### ** Examples

path = example_description_file()
desc = get_remote_info(path)



cleanEx()
nameEx("sys_ext")
### * sys_ext

flush(stderr()); flush(stdout())

### Name: sys_ext
### Title: System Extension
### Aliases: sys_ext

### ** Examples

sys_ext()



cleanEx()
nameEx("tag_table")
### * tag_table

flush(stderr()); flush(stdout())

### Name: tag_table
### Title: Tag Table
### Aliases: tag_table

### ** Examples

repo = "stnava/ANTsR"
tag_table(repo)
tag_table("muschellij2/ANTsR")
tag_table("cran/psych@084bdd0ae2630cf31c26d97a6e13e59d3f0f66e6")



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
