#' Update the generated feed statistics block in README.md.
#'
#' Usage: Rscript scripts/update_stats.R

source("R/aemet.R")
source("R/store.R")

README_PATH <- "README.md"
STATS_START <- "<!-- feed-stats:start -->"
STATS_END <- "<!-- feed-stats:end -->"

con <- db_connect(Sys.getenv("WEATHER_DB", unset = DB_PATH_DEFAULT))
on.exit(dbDisconnect(con), add = TRUE)
coverage <- db_coverage(con)

completed_heatmaps <- if (dir.exists("plots")) {
  list.files("plots", pattern = "^aemet-[0-9]{4}-[0-9]{2}\\.png$", full.names = FALSE)
} else {
  character()
}

format_station <- function(row) {
  first <- as.POSIXct(row$first_obs, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  last <- as.POSIXct(row$last_obs, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  hours <- as.numeric(difftime(last, first, units = "hours"))
  expected <- floor(hours) + 1
  percent <- if (expected > 0) 100 * row$n / expected else 100
  sprintf(
    "- **%s** (`%s`): %s observations, %s to %s UTC, %.1f%% hourly coverage",
    row$ubi, row$idema, format(row$n, big.mark = ",", scientific = FALSE),
    sub("T.*", "", row$first_obs), sub("T.*", "", row$last_obs), percent
  )
}

station_lines <- if (nrow(coverage)) {
  vapply(seq_len(nrow(coverage)), function(i) format_station(coverage[i, ]), character(1))
} else {
  "- No observations stored yet."
}

stats <- c(
  STATS_START,
  "_Generated from the active SQLite snapshot in Cloudflare R2._",
  station_lines,
  sprintf("- Completed heatmaps retained: **%d**", length(completed_heatmaps)),
  STATS_END
)

lines <- readLines(README_PATH, warn = FALSE, encoding = "UTF-8")
start <- match(STATS_START, lines)
end <- match(STATS_END, lines)
if (is.na(start) || is.na(end) || end < start) {
  stop("README.md is missing a valid feed statistics marker block.", call. = FALSE)
}

before <- if (start > 1) lines[seq_len(start - 1)] else character()
after <- if (end < length(lines)) lines[(end + 1):length(lines)] else character()
updated <- c(before, stats, after)
writeLines(updated, README_PATH, useBytes = TRUE)
message("updated feed stats: ", paste(station_lines, collapse = "; "))
