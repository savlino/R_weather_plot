#' Clean up one completed local month after retaining its heatmap.
#'
#' Usage: Rscript scripts/cleanup_month.R <year> <month> [dry_run]

source("R/aemet.R")
source("R/store.R")
source("R/plot_heatmap.R")
source("R/config.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript scripts/cleanup_month.R <year> <month> [dry_run]", call. = FALSE)
}

year <- suppressWarnings(as.integer(args[1]))
month <- suppressWarnings(as.integer(args[2]))
dry_run <- if (length(args) >= 3) tolower(args[3]) %in% c("true", "1", "yes") else TRUE

if (is.na(year) || is.na(month) || month < 1 || month > 12) {
  stop("year and month must identify a valid calendar month.", call. = FALSE)
}

month_start_local <- as.POSIXct(sprintf("%04d-%02d-01 00:00:00", year, month), tz = "Europe/Madrid")
next_start_local <- seq(month_start_local, by = "1 month", length.out = 2)[2]
current_start_local <- as.POSIXct(format(Sys.Date(), "%Y-%m-01 00:00:00"), tz = "Europe/Madrid")

if (month_start_local >= current_start_local) {
  stop("Refusing to clean up the current or a future month.", call. = FALSE)
}

utc_text <- function(value) format(value, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
month_start_utc <- utc_text(month_start_local)
next_start_utc <- utc_text(next_start_local)

con <- db_connect(Sys.getenv("WEATHER_DB", unset = DB_PATH_DEFAULT))
on.exit(dbDisconnect(con), add = TRUE)

rows <- dbGetQuery(
  con,
  "SELECT * FROM observations WHERE fint >= ? AND fint < ? ORDER BY fint, idema",
  params = list(month_start_utc, next_start_utc)
)

if (nrow(rows) == 0) {
  stop(sprintf("No observations found for %04d-%02d.", year, month), call. = FALSE)
}

plot_path <- file.path("plots", sprintf("aemet-%04d-%02d.png", year, month))

cat(sprintf(
  "Month: %04d-%02d\nRows: %d\nUTC range: %s to %s\nDry run: %s\nPlot: %s\n",
  year, month, nrow(rows), month_start_utc, next_start_utc, dry_run,
  plot_path
))

obs <- db_read_observations(
  con, from = month_start_local, to = next_start_local,
  tz = "Europe/Madrid"
)

if (length(unique(obs$idema)) != 1) {
  stop("The cleanup currently expects one station. Run one station per cleanup.", call. = FALSE)
}

dir.create("plots", showWarnings = FALSE)
plot_monthly_heatmap(
  obs, parameter = "ta", year = year, month = month,
  filename = plot_path
)

if (dry_run) {
  message("Dry run complete: no deletion or database upload performed.")
  quit(save = "no", status = 0)
}

DBI::dbExecute(con, "DELETE FROM observations WHERE fint >= ? AND fint < ?",
               params = list(month_start_utc, next_start_utc))
DBI::dbExecute(con, "VACUUM")

message(sprintf("Removed %d rows from the active database.", nrow(rows)))
