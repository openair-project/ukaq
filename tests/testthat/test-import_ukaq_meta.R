test_that("metadata urls are returned", {
  expect_equal(
    meta_url("aurn"),
    "https://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData"
  )
  expect_equal(
    meta_url("saqn"),
    "https://www.scottishairquality.scot/openair/R_data/SCOT_metadata.RData"
  )
  expect_equal(
    meta_url("niaqn"),
    "https://www.airqualityni.co.uk/openair/R_data/NI_metadata.RData"
  )
  expect_equal(
    meta_url("waqn"),
    "https://airquality.gov.wales/sites/default/files/openair/R_data/WAQ_metadata.RData"
  )
  expect_equal(
    meta_url("aqe"),
    "https://airqualityengland.co.uk/assets/openair/R_data/AQE_metadata.RData"
  )
  expect_equal(
    meta_url("lmam"),
    "https://uk-air.defra.gov.uk/openair/LMAM/R_data/LMAM_metadata.RData"
  )
})

test_that("metadata is formatted", {
  # default args
  fmtdata <- formatMeta(rawMeta, year = NA, by_pollutant = FALSE)

  expect_equal(
    names(fmtdata),
    c(
      "source",
      "code",
      "site",
      "site_type",
      "latitude",
      "longitude",
      "start_date",
      "end_date",
      "zone",
      "agglomeration",
      "zagglom",
      "local_authority",
      "lmam_provider",
      "lmam_code"
    )
  )

  expect_equal(
    names(fmtdata),
    tolower(names(fmtdata))
  )

  colclasses <- sapply(fmtdata, class) |> unname()

  expect_equal(
    colclasses,
    c(
      "character",
      "character",
      "character",
      "character",
      "numeric",
      "numeric",
      "Date",
      "Date",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character"
    )
  )

  expect_equal(nrow(fmtdata), 3L)

  # date filtering

  fmtdataDate <-
    formatMeta(rawMeta, year = 2005:2022, by_pollutant = FALSE)

  expect_equal(nrow(fmtdataDate), 1L)

  # by pollutant

  fmtdataPoll <- formatMeta(rawMeta, year = NA, by_pollutant = TRUE)

  expect_equal(nrow(fmtdataPoll), 15L)

  expect_equal(
    names(fmtdataPoll),
    c(
      "source",
      "code",
      "site",
      "site_type",
      "latitude",
      "longitude",
      "pollutant",
      "start_date",
      "end_date",
      "ratified_to",
      "zone",
      "agglomeration",
      "zagglom",
      "local_authority",
      "lmam_provider",
      "lmam_code"
    )
  )

  colclassesPoll <- sapply(fmtdataPoll, class) |> unname()

  expect_equal(
    colclassesPoll,
    c(
      "character",
      "character",
      "character",
      "character",
      "numeric",
      "numeric",
      "character",
      "Date",
      "Date",
      "Date",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character"
    )
  )
})
