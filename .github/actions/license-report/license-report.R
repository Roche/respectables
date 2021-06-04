#!/usr/bin/env Rscript

suppressPackageStartupMessages(library("jsonlite"))
library("RCurl")
suppressPackageStartupMessages(library("curl"))
library("stringr")
suppressPackageStartupMessages(library("purrr"))

## Check if file exists and get path
check_file_exists = function(dir, fname) {
  fpath <- file.path(dir, fname)
  if (!file.exists(fpath)) {
    stop(fpath, " - File not found", call. = FALSE)
  }
  return(fpath)
}

## Cleans and converts field to vector
clean_description_field = function(field) {
  field <- stringr::str_split(field, ",")[[1]]
  field <- stringr::str_squish(field)
  pkgs <- stringr::str_remove(field, " .*")
  return(pkgs)
}

## Gets direct dependencies for package
pkg_deps = function(pkg) {
  dep <- tools::package_dependencies(pkg,
                                    which = c("Depends", "Imports", "LinkingTo"),
                                    recursive = TRUE)
  dep <- unlist(dep)
  return(unique(c(pkg, dep)))
}

## Get comprehensive list of transitive dependencies for package
transitive_dependencies <- function(dir = ".",
                             fields = c("Depends", "Imports")) {
  fname <- check_file_exists(dir, "DESCRIPTION")
  des <- read.dcf(fname)
  out <- des[, intersect(colnames(des), fields)]
  out <- as.list(out)
  names(out) = NULL

  ## Clean fields and get all transitive dependencies
  pkgs <- purrr::map(out, clean_description_field)
  all_deps <- unlist(purrr::map(pkgs, pkg_deps))
  all_deps <- sort(unique(all_deps))
  inst_pkgs <- installed.packages()
  pkgs <- inst_pkgs[rownames(inst_pkgs) %in% all_deps, "Version"]
  return(data.frame(name=names(pkgs), version=pkgs, row.names=NULL))
}

## Fetch license info for a given package and its version from Bioc/CRAN
license_info <- function(pkg, version) {
  crandb_url <- paste0("http://crandb.r-pkg.org/", pkg, "/", version)

  ## TODO: Figure out a way to get Bioc package metadata for a given version of the package
  bioc_home <- "https://www.bioconductor.org/packages/release/"
  packages_uri <- "/src/contrib/PACKAGES"
  bioc_soft_packages <- url(paste0(bioc_home, "bioc", packages_uri))
  bioc_exp_packages <- url(paste0(bioc_home, "data/experiment", packages_uri))
  bioc_ano_packages <- url(paste0(bioc_home, "data/annotation", packages_uri))

  fields_to_read <- c("Package", "Version", "License")
  bioc_soft_df <- read.dcf(bioc_soft_packages, fields = fields_to_read)
  bioc_exp_df <- read.dcf(bioc_exp_packages, fields = fields_to_read)
  bioc_ano_df <- read.dcf(bioc_ano_packages, fields = fields_to_read)

  bioc_df <- rbind.data.frame(bioc_soft_df, bioc_exp_df, bioc_ano_df,
    stringsAsFactors = FALSE
  )

  if (RCurl::url.exists(crandb_url)) {
    pkg_json <- jsonlite::fromJSON(readLines(curl::curl(crandb_url)))
    pkg_license <- pkg_json$License
  } else if (pkg %in% bioc_df$Package) {
    pkg_license <- bioc_df[bioc_df$Package == pkg, "License"]
  } else {
    pkg_license <- "Not in Bioc/CRAN"
  }

  return(pkg_license)
}

## Get licenses for all dependencies
main <- function() {
  args = commandArgs(trailingOnly=TRUE)
  cat("Fetching dependencies...", sep = "\n")
  deps <- transitive_dependencies(dir = args[1])
  cat("Fetching license information. Please be patient as this may take a while...", sep = "\n")
  for (d in 1:nrow(deps)) {
    license <- license_info(deps[d, "name"], deps[d, "version"])
    print(paste(deps[d, "name"], deps[d, "version"], license, sep = " | "))
  }
}

# Execute main
suppressMessages(suppressWarnings(main()))
