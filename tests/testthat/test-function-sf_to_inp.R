#library(testthat)

test_that("sf_to_inp() works", {

  f <- swmmr:::sf_to_inp
  
  expect_error(suppressWarnings(f()))

})
