test_that("annual works", {
  # default
  default_test <- formatSummary(
    rawSummaryAnnual,
    NULL,
    source = c("aurn", "lmam"),
    data_type = "annual",
    pollutant = NULL,
    append_metadata = FALSE,
    metadata_columns = c("latitude", "longitude"),
    pivot = "wide"
  )

  expect_contains(names(default_test),
                  c("source", "date", "year", "code", "site"))

  # codes
  codetest <- formatSummary(
    rawSummaryAnnual,
    code = c("abd", "abd7"),
    source = c("aurn", "lmam"),
    data_type = "annual",
    pollutant = NULL,
    append_metadata = FALSE,
    metadata_columns = c("latitude", "longitude"),
    pivot = "wide"
  )

  expect_equal(nrow(codetest), 2L)

  # pollutants
  polltest <- formatSummary(
    rawSummaryAnnual,
    code = NULL,
    source = c("aurn", "lmam"),
    data_type = "annual",
    pollutant = "no2",
    append_metadata = FALSE,
    metadata_columns = c("latitude", "longitude"),
    pivot = "wide"
  )

  expect_equal(names(polltest),
               c("source", "date", "year", "code", "site", "no2", "no2.capture"))

  # pivot
  longtest <- formatSummary(
    rawSummaryAnnual,
    code = NULL,
    source = c("aurn", "lmam"),
    data_type = "annual",
    pollutant = NULL,
    append_metadata = FALSE,
    metadata_columns = c("latitude", "longitude"),
    pivot = "long"
  )

  expect_equal(
    names(longtest),
    c(
      "source",
      "date",
      "year",
      "code",
      "site",
      "pollutant",
      "mean",
      "capture"
    )
  )

  longtest2 <- formatSummary(
    rawSummaryAnnual,
    code = NULL,
    source = c("aurn", "lmam"),
    data_type = "annual",
    pollutant = c("no2", "o3"),
    append_metadata = FALSE,
    metadata_columns = c("latitude", "longitude"),
    pivot = "long"
  )

  expect_equal(nrow(longtest2), 20L)
  expect_in(longtest2$pollutant, c("no2", "o3"))

})

test_that("monthly works", {
  # default
  default_test <- formatSummary(
    rawSummaryMonthly,
    NULL,
    source = c("aurn", "lmam"),
    data_type = "monthly",
    pollutant = NULL,
    append_metadata = FALSE,
    metadata_columns = c("latitude", "longitude"),
    pivot = "wide"
  )

  expect_contains(
    names(default_test),
    c(
      "source",
      "date",
      "year",
      "month",
      "month_label",
      "code",
      "site"
    )
  )

  # codes
  codetest <- formatSummary(
    rawSummaryMonthly,
    code = c("abd", "abd7"),
    source = c("aurn", "lmam"),
    data_type = "monthly",
    pollutant = NULL,
    append_metadata = FALSE,
    metadata_columns = c("latitude", "longitude"),
    pivot = "wide"
  )

  expect_equal(nrow(codetest), 2L)

  # pollutants
  polltest <- formatSummary(
    rawSummaryMonthly,
    code = NULL,
    source = c("aurn", "lmam"),
    data_type = "monthly",
    pollutant = "no2",
    append_metadata = FALSE,
    metadata_columns = c("latitude", "longitude"),
    pivot = "wide"
  )

  expect_equal(
    names(polltest),
    c(
      "source",
      "date",
      "year",
      "month",
      "month_label",
      "code",
      "site",
      "no2",
      "no2.capture"
    )
  )

  # pivot
  longtest <- formatSummary(
    rawSummaryMonthly,
    code = NULL,
    source = c("aurn", "lmam"),
    data_type = "monthly",
    pollutant = NULL,
    append_metadata = FALSE,
    metadata_columns = c("latitude", "longitude"),
    pivot = "long"
  )

  expect_equal(
    names(longtest),
    c(
      "source",
      "date",
      "year",
      "month",
      "month_label",
      "code",
      "site",
      "pollutant",
      "mean",
      "capture"
    )
  )

  longtest2 <- formatSummary(
    rawSummaryMonthly,
    code = NULL,
    source = c("aurn", "lmam"),
    data_type = "monthly",
    pollutant = c("no2", "o3"),
    append_metadata = FALSE,
    metadata_columns = c("latitude", "longitude"),
    pivot = "long"
  )

  expect_equal(nrow(longtest2), 20L)
  expect_in(longtest2$pollutant, c("no2", "o3"))
})

test_that("we can assemble summary URLS", {
  expect_equal(
    summary_url("aurn", "annual", 2020),
    "https://uk-air.defra.gov.uk/openair/R_data/summary_annual_AURN_2020.rds"
  )

  expect_equal(
    summary_url("lmam", "monthly", 2020),
    "https://uk-air.defra.gov.uk/openair/LMAM/R_data/summary_monthly_LMAM_2020.rds"
  )
})
