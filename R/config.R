#' Project configuration.
#'
#' Override AEMET_STATIONS with a comma- or space-separated list to fetch
#' different or multiple stations without editing the R scripts.
AEMET_DEFAULT_STATION <- "1083L"

configured_stations <- function() {
  value <- Sys.getenv("AEMET_STATIONS", unset = AEMET_DEFAULT_STATION)
  stations <- strsplit(value, "[,[:space:]]+")[[1]]
  stations[nzchar(stations)]
}

configured_station <- function() {
  configured_stations()[1]
}
