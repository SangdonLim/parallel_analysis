#' install a specific package version
#'
#' @export
get_package <- function(pkg_name, pkg_version = NULL) {

  repo_path <- "http://cran.us.r-project.org"

  pkg_list <- installed.packages()
  if (pkg_name %in% pkg_list[, "Package"]) {
    if (is.null(pkg_version)) {
      message(sprintf("%s: exists, checking for updates", pkg_name))
      update.packages(oldPkgs = pkg_name, ask = FALSE, type = "source")
    } else if (packageVersion(pkg_name) != pkg_version) {
      message(sprintf("%s: versions different, reinstalling", pkg_name))
      try(unloadNamespace(pkg_name))
      remotes::install_version(pkg_name, version = pkg_version, repos = repo_path, upgrade = "never")
    } else if (packageVersion(pkg_name) == pkg_version) {
      message(sprintf("%s: versions identical, skipping", pkg_name))
    }
  } else {
    message(sprintf("%s: not found, installing", pkg_name))
    if (is.null(pkg_version)) {
      install.packages(pkg_name)
    } else {
      remotes::install_version(pkg_name, version = pkg_version, repos = repo_path, upgrade = "never")
    }
  }

}
