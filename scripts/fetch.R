#' Poll AEMET for the last 12 hours of observations and append them to SQLite.
#'
#' Usage: Rscript scripts/fetch.R [idema ...]
#' AEMET_API_KEY must be present in the environment.

source("R/aemet.R")
source("R/store.R")

args <- commandArgs(trailingOnly = TRUE)
# Default station: 1083L = CASTRO URDIALES-EDAR (Cantabria).
stations <- if (length(args)) args else strsplit(
  Sys.getenv("AEMET_STATIONS", unset = "1083L"), "[,[:space:]]+"
)[[1]]

con <- db_connect(Sys.getenv("WEATHER_DB", unset = DB_PATH_DEFAULT))
on.exit(dbDisconnect(con), add = TRUE)

for (idema in stations) {
  obs <- aemet_fetch_station(idema)
  written <- db_upsert_observations(con, obs)
  message(sprintf("%s: fetched %d rows, stored %d", idema, nrow(obs), written))
}

print(db_coverage(con))
