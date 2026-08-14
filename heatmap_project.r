#' Interactive entry point: fetch the latest AEMET window, then plot the month.
#'
#' Data source is AEMET OpenData instead of the one-off Meteogalicia CSV
#' export. AEMET's station endpoint only exposes the last ~12 hours, so history
#' is accumulated in a local SQLite store.
#'
#' Dependencies (install once):
#' install.packages(c("httr2", "jsonlite", "DBI", "RSQLite",
#'                    "dplyr", "lubridate", "reshape2", "pheatmap"))

source("R/aemet.R")
source("R/store.R")
source("R/plot_heatmap.R")

# CASTRO URDIALES-EDAR (Cantabria); hourly observations.
IDEMA <- "1083L"

con <- db_connect()

# Top up the store with whatever the API currently exposes.
db_upsert_observations(con, aemet_fetch_station(IDEMA))
print(db_coverage(con))

obs <- db_read_observations(con, idema = IDEMA)

plot_monthly_heatmap(
  obs,
  parameter = "ta",
  year = lubridate::year(Sys.Date()),
  month = lubridate::month(Sys.Date())
)

dbDisconnect(con)
