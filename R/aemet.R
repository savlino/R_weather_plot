#' AEMET OpenData client for conventional surface observations.
#'
#' Endpoint: /api/observacion/convencional/datos/estacion/{idema}
#' It returns the last ~12 hours of observations for one station.
#' AEMET uses a two-step protocol: the first call returns a JSON envelope
#' holding a short-lived `datos` URL, the payload lives behind that URL.

library(httr2)
library(jsonlite)

AEMET_BASE <- "https://opendata.aemet.es/opendata/api"

#' Numeric observation fields kept in the store. Anything else that AEMET
#' sends is preserved verbatim in the `raw` JSON column.
AEMET_NUMERIC_FIELDS <- c(
  "lat", "lon", "alt",
  "ta", "tamin", "tamax", "tpr", "ts", "tss5cm", "tss20cm",
  "hr", "pres", "pres_nmar",
  "prec", "nieve",
  "vv", "vmax", "dv", "dmax", "stdvv", "stddv",
  "inso", "rviento", "vis"
)

AEMET_TEXT_FIELDS <- c("idema", "ubi")

aemet_api_key <- function() {
  key <- Sys.getenv("AEMET_API_KEY", unset = "")
  if (!nzchar(key)) {
    stop(
      "AEMET_API_KEY is not set. Put it in ~/.Renviron or the environment, ",
      "never in tracked source files.",
      call. = FALSE
    )
  }
  key
}

#' Perform a request against AEMET with retry on throttling (HTTP 429).
aemet_get <- function(url, api_key, encoding = "UTF-8") {
  resp <- request(url) |>
    req_headers(api_key = api_key, Accept = "application/json") |>
    req_user_agent("R_weather_plot (https://github.com/)") |>
    req_retry(
      max_tries = 5,
      is_transient = function(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504)
    ) |>
    req_perform()

  resp_body_string(resp, encoding = encoding)
}

#' Fetch the last 12 hours of observations for a station.
#'
#' @param idema AEMET station code.
#' @return data.frame in AEMET's wide format, one row per timestamp.
aemet_fetch_station <- function(idema, api_key = aemet_api_key()) {
  url <- sprintf(
    "%s/observacion/convencional/datos/estacion/%s",
    AEMET_BASE, utils::URLencode(idema, reserved = TRUE)
  )

  envelope <- fromJSON(aemet_get(url, api_key))

  if (!identical(as.integer(envelope$estado), 200L)) {
    stop(
      sprintf(
        "AEMET returned estado=%s: %s",
        envelope$estado, envelope$descripcion %||% "no description"
      ),
      call. = FALSE
    )
  }

  # The payload is served as ISO-8859-15, station names carry accents.
  payload <- aemet_get(envelope$datos, api_key, encoding = "ISO-8859-15")
  obs <- fromJSON(payload, simplifyDataFrame = TRUE)

  if (!is.data.frame(obs) || nrow(obs) == 0) {
    return(aemet_empty_frame())
  }

  aemet_normalize(obs)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

aemet_empty_frame <- function() {
  frame <- data.frame(
    fint = character(0), idema = character(0), ubi = character(0),
    stringsAsFactors = FALSE
  )
  for (field in AEMET_NUMERIC_FIELDS) frame[[field]] <- numeric(0)
  frame$raw <- character(0)
  frame
}

#' Coerce AEMET's payload into a stable set of columns and types.
#'
#' Fields are optional in the API response: a station without a snow sensor
#' simply omits `nieve`, so missing columns are filled with NA rather than
#' being treated as an error.
aemet_normalize <- function(obs) {
  raw <- vapply(
    seq_len(nrow(obs)),
    function(i) toJSON(obs[i, , drop = FALSE], auto_unbox = TRUE, na = "null"),
    character(1)
  )

  out <- data.frame(
    fint = format(
      as.POSIXct(obs$fint, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
      "%Y-%m-%dT%H:%M:%SZ"
    ),
    stringsAsFactors = FALSE
  )

  for (field in AEMET_TEXT_FIELDS) {
    out[[field]] <- if (is.null(obs[[field]])) NA_character_ else as.character(obs[[field]])
  }
  for (field in AEMET_NUMERIC_FIELDS) {
    out[[field]] <- if (is.null(obs[[field]])) NA_real_ else as.numeric(obs[[field]])
  }

  out$raw <- raw
  out[!is.na(out$fint) & !is.na(out$idema), ]
}
