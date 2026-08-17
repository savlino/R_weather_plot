project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(project_root, "R", "aemet.R"))
source(file.path(project_root, "R", "store.R"))
source(file.path(project_root, "R", "plot_heatmap.R"))

make_observations <- function(times = c(
  "2026-08-10T00:00:00Z",
  "2026-08-10T01:00:00Z",
  "2026-08-10T02:00:00Z"
)) {
  obs <- data.frame(
    fint = times,
    idema = rep("1083L", length(times)),
    ubi = rep("TEST STATION", length(times)),
    stringsAsFactors = FALSE
  )
  for (field in AEMET_NUMERIC_FIELDS) obs[[field]] <- NA_real_
  obs$ta <- seq(20, by = 1, length.out = length(times))
  obs$prec <- seq(0.1, by = 0.1, length.out = length(times))
  obs$raw <- sprintf('{"fint":"%s","ta":%s}', obs$fint, obs$ta)
  obs
}
