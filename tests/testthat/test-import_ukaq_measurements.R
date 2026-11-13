test_that("ratification cutoffs cover the whole ratified_to day", {
  local_mocked_bindings(
    import_ukaq_meta = function(...) {
      data.frame(
        source = "aurn",
        code = c("ABC", "ABC", "ABC", "XYZ"),
        pollutant = c("no2", "no2", "o3", "no2"),
        ratified_to = as.Date(c("2024-06-30", "2024-12-31", NA, "2024-01-01"))
      )
    }
  )

  cutoffs <- ratification_cutoffs("aurn")

  # one row per site-pollutant, NA ratification dates dropped
  expect_equal(nrow(cutoffs), 2L)

  # duplicates conservatively use the earliest date
  abc <- cutoffs$cutoff[cutoffs$code == "ABC"]
  expect_equal(abc, as.POSIXct("2024-07-01", tz = "GMT"))

  # the last hour of the ratified_to day is ratified, the next is not
  expect_true(as.POSIXct("2024-06-30 23:00", tz = "GMT") < abc)
  expect_false(as.POSIXct("2024-07-01 00:00", tz = "GMT") < abc)
})
