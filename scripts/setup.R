#' Install the packages the project needs and report what is still missing.
#'
#' Usage: Rscript scripts/setup.R          # check only
#'        Rscript scripts/setup.R install  # check and install

REQUIRED <- c(
  "httr2", "jsonlite", "DBI", "RSQLite",
  "dplyr", "lubridate", "reshape2", "pheatmap", "testthat"
)

do_install <- "install" %in% commandArgs(trailingOnly = TRUE)

lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(lib) && !dir.exists(lib)) dir.create(lib, recursive = TRUE)
if (!nzchar(lib)) lib <- .libPaths()[1]

missing <- setdiff(REQUIRED, rownames(installed.packages()))

if (length(missing) && do_install) {
  install.packages(
    missing, lib = lib,
    repos = "https://cloud.r-project.org", dependencies = TRUE
  )
  missing <- setdiff(REQUIRED, rownames(installed.packages()))
}

for (p in REQUIRED) {
  cat(sprintf("%-10s %s\n", p, if (p %in% missing) "MISSING" else "OK"))
}

if (length(missing)) {
  cat("\nRun: Rscript scripts/setup.R install\n")
} else {
  cat("\nAll dependencies present.\n")
}

key <- Sys.getenv("AEMET_API_KEY", unset = "")
cat(sprintf(
  "AEMET_API_KEY %s\n",
  if (nzchar(key)) "OK" else "NOT SET - add it to ~/.Renviron"
))
