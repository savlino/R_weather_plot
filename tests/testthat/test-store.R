test_that("SQLite upsert is idempotent and revision-aware", {
  db <- file.path(tempdir(), paste0("weather-", Sys.getpid(), ".sqlite"))
  unlink(db)
  con <- db_connect(db)
  on.exit(dbDisconnect(con), add = TRUE)

  obs <- make_observations()
  expect_equal(db_upsert_observations(con, obs), 3)
  expect_equal(db_upsert_observations(con, obs), 0)

  revised <- obs
  revised$ta[1] <- 99
  revised$raw[1] <- '{"fint":"2026-08-10T00:00:00Z","ta":99}'
  expect_equal(db_upsert_observations(con, revised), 1)

  stored <- db_read_observations(con, idema = TEST_IDEMA)
  expect_equal(nrow(stored), 3)
  expect_equal(stored$ta[1], 99)
  expect_equal(length(unique(paste(stored$idema, stored$fint))), 3)
})

test_that("SQLite read converts UTC timestamps to Madrid time", {
  db <- file.path(tempdir(), paste0("weather-tz-", Sys.getpid(), ".sqlite"))
  unlink(db)
  con <- db_connect(db)
  on.exit(dbDisconnect(con), add = TRUE)

  db_upsert_observations(con, make_observations(c("2026-08-10T00:00:00Z")))
  stored <- db_read_observations(con, idema = TEST_IDEMA, tz = "Europe/Madrid")

  expect_s3_class(stored$fint, "POSIXct")
  expect_s3_class(stored$fint_local, "POSIXct")
  expect_equal(format(stored$fint_local, "%H:%M", tz = "Europe/Madrid"), "02:00")
})
