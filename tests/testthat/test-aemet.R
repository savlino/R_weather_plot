test_that("AEMET normalisation keeps optional fields stable", {
  payload <- data.frame(
    fint = c("2026-08-10T01:00:00", "2026-08-10T02:00:00", NA),
    idema = rep(TEST_IDEMA, 3),
    ubi = c("A Coru\u00f1a", "A Coru\u00f1a", "A Coru\u00f1a"),
    ta = c("20.5", "21.0", "22.0"),
    stringsAsFactors = FALSE
  )

  normalised <- aemet_normalize(payload)

  expect_equal(nrow(normalised), 2)
  expect_equal(normalised$fint, c("2026-08-10T01:00:00Z", "2026-08-10T02:00:00Z"))
  expect_equal(normalised$idema, rep(TEST_IDEMA, 2))
  expect_equal(normalised$ubi, c("A Coru\u00f1a", "A Coru\u00f1a"))
  expect_equal(normalised$ta, c(20.5, 21))
  expect_true(all(is.na(normalised$hr)))
})
