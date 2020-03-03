library("ggeocode")
context("opencage_basic")

test_that("opencage returns a list",{
  expect_is(opencage(placename="100 Main, Houston, Tx, 77008, USA",
                     keyfile="/home/ajackson/AccessTokens.txt"), "list")
})
