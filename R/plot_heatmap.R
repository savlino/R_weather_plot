#' Monthly heatmap built from the accumulated AEMET observation feed.
#'
#' Replaces the original Meteogalicia CSV pipeline. The two sources differ in
#' shape: Meteogalicia shipped long data (one row per parameter per timestamp,
#' `Código.parámetro` / `Valor`), AEMET ships wide data (one row per timestamp,
#' one column per parameter), so no filter/pivot on parameter name is needed.

library(dplyr)
library(lubridate)
library(reshape2)
library(pheatmap)

BIN_LABELS <- c(
  "0:00-3:00", "3:00-6:00", "6:00-9:00", "9:00-12:00",
  "12:00-15:00", "15:00-18:00", "18:00-21:00", "21:00-0:00"
)

PARAMETER_LABELS <- list(
  ta   = list(title = "Temperature", unit = "\u00baC"),
  hr   = list(title = "Relative humidity", unit = "%"),
  pres = list(title = "Pressure", unit = "hPa"),
  prec = list(title = "Precipitation", unit = "L/m2"),
  vv   = list(title = "Wind speed", unit = "m/s")
)

#' Aggregate observations into a day x 3-hour-bin matrix.
#'
#' @param obs data.frame from `db_read_observations()`.
#' @param parameter Column to aggregate, e.g. "ta".
#' @param year,month Calendar month to render, in the local timezone of `obs`.
#' @param agg Aggregation applied within each bin; `sum` suits precipitation.
build_bin_matrix <- function(obs, parameter = "ta", year, month, agg = mean) {
  stopifnot(parameter %in% names(obs))

  binned <- obs %>%
    filter(
      lubridate::year(fint_local) == year,
      lubridate::month(fint_local) == month,
      !is.na(.data[[parameter]])
    ) %>%
    mutate(
      day = lubridate::day(fint_local),
      bin = cut(
        lubridate::hour(fint_local),
        breaks = c(-Inf, 2, 5, 8, 11, 14, 17, 20, Inf),
        labels = BIN_LABELS
      )
    ) %>%
    group_by(day, bin) %>%
    summarise(value = agg(.data[[parameter]]), .groups = "drop")

  if (nrow(binned) == 0) {
    stop(sprintf("No '%s' observations stored for %04d-%02d.", parameter, year, month),
         call. = FALSE)
  }

  mat <- acast(binned, bin ~ day, value.var = "value", drop = FALSE)

  # Gaps in the feed must stay visible as empty cells, so every bin and every
  # day of the month is materialised even when nothing was recorded.
  days <- seq_len(lubridate::days_in_month(make_date(year, month, 1)))
  full <- matrix(
    NA_real_, nrow = length(BIN_LABELS), ncol = length(days),
    dimnames = list(BIN_LABELS, as.character(days))
  )
  full[rownames(mat), colnames(mat)] <- mat
  full
}

#' Render (and optionally save) the monthly heatmap.
plot_monthly_heatmap <- function(obs, parameter = "ta", year, month,
                                 station_label = NULL, agg = mean,
                                 filename = NA) {
  mat <- build_bin_matrix(obs, parameter, year, month, agg)

  meta <- PARAMETER_LABELS[[parameter]]
  if (is.null(meta)) meta <- list(title = parameter, unit = "")

  if (is.null(station_label)) {
    known <- unique(c(obs$ubi[!is.na(obs$ubi)], obs$idema[!is.na(obs$idema)]))
    station_label <- if (length(known)) known[1] else "unknown station"
  }

  title <- sprintf(
    "%s in %s (%s %d), %s",
    meta$title, station_label,
    format(make_date(year, month, 1), "%B"), year, meta$unit
  )

  pheatmap(
    mat, main = title,
    treeheight_row = 0, treeheight_col = 0,
    cellwidth = 15, cellheight = 20,
    cluster_rows = FALSE, cluster_cols = FALSE,
    angle_col = 0, na_col = "grey92",
    filename = filename
  )
}
