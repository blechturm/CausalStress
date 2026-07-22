#!/usr/bin/env Rscript

qs_version <- "0.27.3"
stringfish_version <- "0.17.0"
repos <- "https://cloud.r-project.org"

has_version <- function(pkg, version) {
  requireNamespace(pkg, quietly = TRUE) &&
    identical(as.character(utils::packageVersion(pkg)), version)
}

install_cran <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    install.packages(missing, repos = repos)
  }
}

install_archive <- function(pkg, version) {
  url <- sprintf(
    "https://cran.r-project.org/src/contrib/Archive/%s/%s_%s.tar.gz",
    pkg,
    pkg,
    version
  )
  install.packages(url, repos = NULL, type = "source")
}

if (has_version("qs", qs_version)) {
  cat(
    sprintf("qs=%s\n", as.character(utils::packageVersion("qs"))),
    sprintf("stringfish=%s\n", as.character(utils::packageVersion("stringfish"))),
    sep = ""
  )
  quit(save = "no", status = 0)
}

install_cran(c("Rcpp", "RcppParallel", "RApiSerialize", "BH"))

if (!has_version("stringfish", stringfish_version)) {
  install_archive("stringfish", stringfish_version)
}

if (!has_version("qs", qs_version)) {
  install_archive("qs", qs_version)
}

cat(
  sprintf("qs=%s\n", as.character(utils::packageVersion("qs"))),
  sprintf("stringfish=%s\n", as.character(utils::packageVersion("stringfish"))),
  sep = ""
)
