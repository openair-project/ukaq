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

  classes <- unname(sapply(fmtdata, class))

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

test_that("codes and sources are paired", {
  nets <- ukaq_network_names

  # length-1 source is recycled
  expect_equal(
    pair_code_source(c("MY1", "KC1"), "aurn", nets)$source,
    c("aurn", "aurn")
  )

  # same-length source is paired
  pairs <- pair_code_source(c("MY1", "ABD"), c("AURN", "ukaq"), nets)
  expect_equal(pairs$source, c("aurn", "ukaq"))

  # duplicate pairs are dropped
  expect_equal(nrow(pair_code_source(c("MY1", "my1"), "aurn", nets)), 1L)

  # mismatched lengths and invalid networks error
  expect_error(
    pair_code_source(c("MY1", "KC1", "ABD"), c("aurn", "saqn"), nets),
    "length 1 or the same length"
  )
  expect_error(pair_code_source("MY1", "woops", nets), "woops")

  # only the needed networks are imported
  expect_equal(pair_networks(pairs, nets), nets)
  expect_equal(
    pair_networks(pair_code_source(c("A", "B"), c("saqn", "aurn"), nets), nets),
    c("aurn", "saqn")
  )
})

test_that("code/source pairs are resolved", {
  nets <- ukaq_network_names
  available <- data.frame(
    code = c("MY1", "ABD", "ABD", "AD1"),
    source = c("aurn", "aurn", "saqn", "lmam")
  )

  # pinned & auto
  pairs <- pair_code_source(
    c("my1", "ABD", "AD1"),
    c("aurn", "saqn", "ukaq"),
    nets
  )
  res <- resolve_code_source(pairs, available, nets)
  expect_equal(res$code, c("MY1", "ABD", "AD1"))
  expect_equal(res$source, c("aurn", "saqn", "lmam"))

  # ambiguous auto
  expect_warning(
    res <- resolve_code_source(
      pair_code_source("ABD", "ukaq", nets),
      available,
      nets
    ),
    "ABD (aurn, saqn)",
    fixed = TRUE
  )
  expect_equal(res$source, "aurn")

  # missing
  expect_warning(
    res <- resolve_code_source(
      pair_code_source(c("MY1", "AD1"), "aurn", nets),
      available,
      nets
    ),
    "AD1 (aurn)",
    fixed = TRUE
  )
  expect_equal(res$code, "MY1")

  expect_error(
    resolve_code_source(pair_code_source("XYZ", "ukaq", nets), available, nets),
    "None of the requested codes"
  )
})
