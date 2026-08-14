#' Render a monthly heatmap from the stored feed.
#'
#' Usage: Rscript scripts/render_heatmap.R <idema> <year> <month> [parameter]

source("R/aemet.R")
source("R/store.R")
source("R/plot_heatmap.R")

args <- commandArgs(trailingOnly = TRUE)
idema <- if (length(args) >= 1) args[1] else "1083L"
year <- if (length(args) >= 2) as.integer(args[2]) else lubridate::year(Sys.Date())
month <- if (length(args) >= 3) as.integer(args[3]) else lubridate::month(Sys.Date())
parameter <- if (length(args) >= 4) args[4] else "ta"

con <- db_connect(Sys.getenv("WEATHER_DB", unset = DB_PATH_DEFAULT))
on.exit(dbDisconnect(con), add = TRUE)

obs <- db_read_observations(con, idema = idema)

dir.create("plots", showWarnings = FALSE)
outfile <- sprintf("plots/%s_%s_%04d-%02d.png", idema, parameter, year, month)

plot_monthly_heatmap(
  obs, parameter = parameter, year = year, month = month,
  agg = if (parameter == "prec") sum else mean,
  filename = outfile
)

message("written: ", outfile)
