library("ggeocode")
context("opencage_basic")

test_that("opencage returns a data frame",{
  expect_is(opencage(placename="100 Main, Houston, Tx, 77008, USA",
                     keyfile="/home/ajackson/AccessTokens.txt"), "data.frame")
})

test_that("compare_addys returns a vector",{
  expect_equal(compare_addys(address_1="100 Main, Houston, Tx 77008, USA",
                             address_2="101 Main, Houston, Tx"),
            c(1,0,0,-1,-1))
})
