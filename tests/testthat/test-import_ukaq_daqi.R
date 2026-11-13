test_that("daqi works", {
  default <- formatDAQI(
    rawDAQI,
    pollutant = NULL,
    code = NULL,
    source = "aurn",
    pivot = "long",
    append_metadata = FALSE,
    metadata_columns = c("zagglom"),
    daqi_columns = c(
      "concentration",
      "poll_index",
      "poll_band",
      "colour_index",
      "colour_band",
      "measurement_period"
    )
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
      "measurement_period",
      "colour_index",
      "colour_band"
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
      source = "aurn",
      pivot = "long",
      append_metadata = FALSE,
      metadata_columns = c("zagglom"),
      daqi_columns = c(
        "concentration",
        "poll_index",
        "poll_band",
        "colour_index",
        "colour_band",
        "measurement_period"
      )
    )

  expect_in(poll$pollutant, c("no2", "pm10"))

  codes <-
    formatDAQI(
      rawDAQI,
      pollutant = NULL,
      code = c("abd"),
      source = "aurn",
      pivot = "long",
      append_metadata = FALSE,
      metadata_columns = c("zagglom"),
      daqi_columns = c(
        "concentration",
        "poll_index",
        "poll_band",
        "colour_index",
        "colour_band",
        "measurement_period"
      )
    )

  expect_in(codes$code, c("ABD"))

  wide <-
    formatDAQI(
      rawDAQI,
      pollutant = NULL,
      code = NULL,
      source = "aurn",
      pivot = "wide",
      append_metadata = FALSE,
      metadata_columns = c("zagglom"),
      daqi_columns = c(
        "concentration",
        "poll_index",
        "poll_band",
        "colour_index",
        "colour_band",
        "measurement_period"
      )
    )

  expect_equal(
    names(wide),
    c(
      "source",
      "date",
      "code",
      "site",
      "no2",
      "no2_index",
      "no2_band",
      "no2_col_index",
      "no2_col_band",
      "no2_period",
      "pm10",
      "pm10_index",
      "pm10_band",
      "pm10_col_index",
      "pm10_col_band",
      "pm10_period",
      "pm2.5",
      "pm2.5_index",
      "pm2.5_band",
      "pm2.5_col_index",
      "pm2.5_col_band",
      "pm2.5_period"
    )
  )

  expect_type(wide$no2, "double")
  expect_type(wide$no2_index, "integer")
  expect_s3_class(wide$no2_band, "factor")
  expect_type(wide$no2_col_index, "character")
  expect_type(wide$no2_col_band, "character")
  expect_type(wide$no2_period, "character")

  wide2 <-
    formatDAQI(
      rawDAQI,
      pollutant = c("no2", "pm10"),
      code = NULL,
      source = "aurn",
      pivot = "wide",
      append_metadata = FALSE,
      metadata_columns = c("zagglom"),
      daqi_columns = c(
        "concentration",
        "poll_index",
        "poll_band",
        "colour_index",
        "colour_band",
        "measurement_period"
      )
    )

  expect_equal(
    names(wide2),
    c(
      "source",
      "date",
      "code",
      "site",
      "no2",
      "no2_index",
      "no2_band",
      "no2_col_index",
      "no2_col_band",
      "no2_period",
      "pm10",
      "pm10_index",
      "pm10_band",
      "pm10_col_index",
      "pm10_col_band",
      "pm10_period"
    )
  )
})

test_that("we can construct DAQI URLS", {
  expect_equal(
    daqi_url("aurn", 2020),
    "https://uk-air.defra.gov.uk/openair/R_data/annual_DAQI_AURN_2020.rds"
  )
})

test_that("missing DAQI indices give missing bands and colours", {
  raw <- rawDAQI
  raw$poll_index[1] <- NA

  out <- formatDAQI(
    raw,
    pollutant = NULL,
    code = NULL,
    source = "aurn",
    pivot = "long",
    append_metadata = FALSE,
    metadata_columns = c("zagglom"),
    daqi_columns = c("poll_index", "poll_band", "colour_index", "colour_band")
  )

  na_row <- out[is.na(out$poll_index), ]
  expect_equal(nrow(out), nrow(raw))
  expect_true(is.na(na_row$poll_band))
  expect_true(is.na(na_row$colour_index))
  expect_true(is.na(na_row$colour_band))
})

test_that("colour_band can be returned without poll_band", {
  out <- formatDAQI(
    rawDAQI,
    pollutant = NULL,
    code = NULL,
    source = "aurn",
    pivot = "long",
    append_metadata = FALSE,
    metadata_columns = c("zagglom"),
    daqi_columns = c("concentration", "colour_band")
  )

  expect_true("colour_band" %in% names(out))
  expect_false("poll_band" %in% names(out))
  expect_false(anyNA(out$colour_band))
})
