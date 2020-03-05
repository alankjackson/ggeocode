library("ggeocode")
context("census_basic")

test_that("census returns a data frame",{
  expect_is(census(placename="100 Main, Houston, Tx, 77008, USA"), "data.frame")
})
