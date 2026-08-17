test_that("heatmap matrix materialises all days and bins", {
  obs <- make_observations(c(
    "2026-08-10T00:00:00Z",
    "2026-08-10T01:00:00Z",
    "2026-08-10T02:00:00Z"
  ))
  obs$fint <- as.POSIXct(obs$fint, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  obs$fint_local <- as.POSIXct(format(obs$fint, tz = "Europe/Madrid"), tz = "Europe/Madrid")

  matrix <- build_bin_matrix(obs, parameter = "ta", year = 2026, month = 8)

  expect_equal(dim(matrix), c(8, 31))
  expect_equal(sum(!is.na(matrix)), 2)
  expect_true(is.na(matrix[1, 31]))
})

test_that("precipitation can be summed within a bin", {
  obs <- make_observations(c(
    "2026-08-09T22:00:00Z",
    "2026-08-09T23:00:00Z",
    "2026-08-10T00:00:00Z"
  ))
  obs$fint <- as.POSIXct(obs$fint, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  obs$fint_local <- as.POSIXct(format(obs$fint, tz = "Europe/Madrid"), tz = "Europe/Madrid")

  matrix <- build_bin_matrix(obs, parameter = "prec", year = 2026, month = 8, agg = sum)

  expect_equal(matrix[1, 10], 0.6)
})
