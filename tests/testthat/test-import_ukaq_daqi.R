test_that("daqi works", {
  default <- formatDAQI(
    rawDAQI,
    pollutant = NULL,
    code = NULL,
    pivot = "long",
    append_metadata = FALSE,
    metadata_columns = c("zagglom")
  )

  expect_equal(nrow(default), nrow(rawDAQI))
  expect_equal(
    names(default),
    c(
      "source",
      "date",
      "code",
      "site",
      "pollutant",
      "concentration",
      "poll_index",
      "poll_band",
      "measurement_period"
    )
  )

  expect_s3_class(default$date, "Date")
  expect_s3_class(default$poll_band, "factor")
  expect_type(default$concentration, "double")

  poll <-
    formatDAQI(
      rawDAQI,
      pollutant = c("no2", "pm10"),
      code = NULL,
      pivot = "long",
      append_metadata = FALSE,
      metadata_columns = c("zagglom")
    )

  expect_in(poll$pollutant, c("no2", "pm10"))

  codes <-
    formatDAQI(
      rawDAQI,
      pollutant = NULL,
      code = c("abd"),
      pivot = "long",
      append_metadata = FALSE,
      metadata_columns = c("zagglom")
    )

  expect_in(codes$code, c("ABD"))

  wide <-
    formatDAQI(
      rawDAQI,
      pollutant = NULL,
      code = NULL,
      pivot = "wide",
      append_metadata = FALSE,
      metadata_columns = c("zagglom")
    )

  expect_equal(
    names(wide),
    c(
      "date",
      "code",
      "site",
      "no2",
      "no2_index",
      "no2_band",
      "pm10",
      "pm10_index",
      "pm10_band",
      "pm2.5",
      "pm2.5_index",
      "pm2.5_band"
    )
  )

  expect_type(wide$no2, "double")
  expect_type(wide$no2_index, "integer")
  expect_s3_class(wide$no2_band, "factor")

  wide2 <-
    formatDAQI(
      rawDAQI,
      pollutant = c("no2", "pm10"),
      code = NULL,
      pivot = "wide",
      append_metadata = FALSE,
      metadata_columns = c("zagglom")
    )

  expect_equal(
    names(wide2),
    c(
      "date",
      "code",
      "site",
      "no2",
      "no2_index",
      "no2_band",
      "pm10",
      "pm10_index",
      "pm10_band"
    )
  )
})

test_that("we can construct DAQI URLS", {
  expect_equal(
    daqi_url("aurn", 2020),
    "https://uk-air.defra.gov.uk/openair/R_data/annual_DAQI_AURN_2020.rds"
  )
})
