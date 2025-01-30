test_that("colours are returned", {
  pal <- palette_daqi("index")
  expect_type(pal, "character")
  expect_vector(pal)

  pal2 <- palette_daqi("index", named = TRUE)
  expect_type(pal2, "character")
  expect_named(pal2)
  expect_vector(pal2)

  pal3 <- palette_gaf()
  expect_type(pal3, "character")
  expect_vector(pal3)

  pal4 <- palette_gaf(named = TRUE)
  expect_type(pal4, "character")
  expect_named(pal4)
  expect_vector(pal4)
})
