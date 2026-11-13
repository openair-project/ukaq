## code to prepare `testing` dataset goes here

# meta
rawMeta <- importMeta(c("aurn", "saqn", "lmam"))
rawMeta <- dplyr::slice_head(rawMeta, n = 5, by = source)

# summary
grid <-
  expand.grid(
    year = 2020:2022,
    source = c("aurn", "lmam"),
    stringsAsFactors = FALSE
  )

rawSummaryAnnual <- importSummaries(grid, data_type = "annual")
rawSummaryAnnual <- dplyr::slice_head(rawSummaryAnnual, n = 5, by = source) |>
  janitor::remove_empty("cols")

rawSummaryMonthly <- importSummaries(grid, data_type = "monthly")
rawSummaryMonthly <- dplyr::slice_head(rawSummaryMonthly, n = 5, by = source) |>
  janitor::remove_empty("cols")

# daqi
rawDAQI <- importDAQI(2020:2022, source = c("aurn", "saqn"))
rawDAQI <- dplyr::slice_head(rawDAQI, n = 5, by = source)

usethis::use_data(
  rawMeta,
  rawSummaryAnnual,
  rawSummaryMonthly,
  rawDAQI,
  overwrite = TRUE,
  internal = TRUE
)
