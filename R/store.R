#' SQLite store for the accumulating AEMET observation feed.
#'
#' The station endpoint only exposes the last ~12 hours, so every run has to
#' append into a durable store. (idema, fint) is the primary key, which makes
#' re-runs and overlapping windows idempotent.

library(DBI)
library(RSQLite)

DB_PATH_DEFAULT <- file.path("data", "weather.sqlite")

OBSERVATION_COLUMNS <- c(
  "idema", "fint", "ubi", AEMET_NUMERIC_FIELDS, "raw", "fetched_at"
)

db_connect <- function(path = DB_PATH_DEFAULT) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # Keep the database self-contained because snapshots are moved as one file.
  con <- dbConnect(SQLite(), path)
  db_init(con)
  con
}

db_init <- function(con) {
  numeric_defs <- paste(sprintf("  %s REAL", AEMET_NUMERIC_FIELDS), collapse = ",\n")
  dbExecute(con, sprintf(
    "CREATE TABLE IF NOT EXISTS observations (
      idema TEXT NOT NULL,
      fint TEXT NOT NULL,
      ubi TEXT,
    %s,
      raw TEXT,
      fetched_at TEXT NOT NULL,
      PRIMARY KEY (idema, fint)
    );", numeric_defs
  ))
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_obs_fint ON observations (fint);")
  invisible(con)
}

#' Insert observations, overwriting any row already stored for the same
#' (idema, fint). AEMET does revise recent values, so last write wins.
#'
#' Rows whose payload is byte-identical to what is already stored are skipped,
#' which keeps the snapshot unchanged when a poll brings nothing
#' new.
#'
#' @return number of rows written.
db_upsert_observations <- function(con, obs) {
  if (nrow(obs) == 0) return(0L)

  obs <- db_drop_unchanged(con, obs)
  if (nrow(obs) == 0) return(0L)

  obs$fetched_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  for (col in setdiff(OBSERVATION_COLUMNS, names(obs))) {
    obs[[col]] <- NA
  }
  obs <- obs[, OBSERVATION_COLUMNS, drop = FALSE]

  placeholders <- paste(rep("?", length(OBSERVATION_COLUMNS)), collapse = ", ")
  sql <- sprintf(
    "INSERT OR REPLACE INTO observations (%s) VALUES (%s);",
    paste(OBSERVATION_COLUMNS, collapse = ", "), placeholders
  )

  dbBegin(con)
  on.exit(if (dbIsValid(con)) try(dbRollback(con), silent = TRUE), add = TRUE)
  written <- dbExecute(con, sql, params = unname(as.list(obs)))
  dbCommit(con)
  on.exit()

  written
}

db_drop_unchanged <- function(con, obs) {
  existing <- dbGetQuery(
    con,
    sprintf(
      "SELECT idema, fint, raw FROM observations WHERE fint IN (%s);",
      paste(rep("?", nrow(obs)), collapse = ", ")
    ),
    params = unname(as.list(obs$fint))
  )
  if (nrow(existing) == 0) return(obs)

  key <- function(df) paste(df$idema, df$fint, df$raw, sep = "\u0001")
  obs[!key(obs) %in% key(existing), , drop = FALSE]
}

#' Read observations back as a tidy data.frame with a real POSIXct timestamp.
#'
#' @param tz Timezone used for `fint_local`; AEMET timestamps are UTC.
db_read_observations <- function(con, idema = NULL, from = NULL, to = NULL,
                                 tz = "Europe/Madrid") {
  sql <- "SELECT * FROM observations WHERE 1 = 1"
  params <- list()

  if (!is.null(idema)) {
    sql <- paste(sql, "AND idema = ?")
    params <- c(params, list(idema))
  }
  if (!is.null(from)) {
    sql <- paste(sql, "AND fint >= ?")
    params <- c(params, list(format(as.POSIXct(from, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")))
  }
  if (!is.null(to)) {
    sql <- paste(sql, "AND fint < ?")
    params <- c(params, list(format(as.POSIXct(to, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")))
  }
  sql <- paste(sql, "ORDER BY fint")

  out <- if (length(params)) {
    dbGetQuery(con, sql, params = params)
  } else {
    dbGetQuery(con, sql)
  }

  out$fint <- as.POSIXct(out$fint, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out$fint_local <- as.POSIXct(format(out$fint, tz = tz), tz = tz)
  out
}

db_coverage <- function(con) {
  dbGetQuery(con, "
    SELECT idema, ubi, COUNT(*) AS n, MIN(fint) AS first_obs, MAX(fint) AS last_obs
    FROM observations GROUP BY idema, ubi ORDER BY idema;")
}
