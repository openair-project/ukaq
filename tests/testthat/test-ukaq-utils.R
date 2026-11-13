test_that("'ukaq' gets subbed correctly", {
  expect_equal(
    match_source("ukaq", ukaq_network_names),
    c("aurn", "aqe", "saqn", "waqn", "niaqn", "lmam")
  )

  expect_equal(
    match_source("ukaq", ukaq_network_names_nolocal),
    c("aurn", "aqe", "saqn", "waqn", "niaqn")
  )

  expect_equal(match_source("aurn", ukaq_network_names), c("aurn"))

  expect_error(match_source("woops", ukaq_network_names))

  expect_error(match_source("lmam", ukaq_network_names_nolocal))
})

test_that("'pivot' is correctly matched", {
  expect_equal(match_pivot("longer"), "long")
  expect_equal(match_pivot("long"), "long")
  expect_equal(match_pivot("wider"), "wide")
  expect_equal(match_pivot("wide"), "wide")
  expect_error(match_pivot("anything_else"))
})

test_that("factor conversions work", {
  testdat <-
    data.frame(
      int = 1L:5L,
      date = rep(Sys.Date(), 5L),
      fct = factor(1:5),
      chr = letters[1:5],
      fct2 = factor(letters[1:5])
    )

  fmtdata <- factor_to_char(testdat)

  classes <- sapply(fmtdata, class) |> unname()

  expect_equal(
    classes,
    c("integer", "Date", "character", "character", "character")
  )
})

test_that("table reclassifier works", {
  expect_no_error(tbl_out(iris, .class = NULL))

  expect_s3_class(
    tbl_out(iris, .class = "tbl"),
    c("tbl_df", "tbl", "data.frame")
  )

  expect_s3_class(tbl_out(iris, .class = "df"), c("data.frame"))
})

test_that("we can bind dataframes with different columns", {
  newdf <- rbindNames(list(iris, mtcars))
  expect_equal(names(newdf), c(names(iris), names(mtcars)))
})
