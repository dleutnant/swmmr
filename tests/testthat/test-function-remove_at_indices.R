#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("remove_at_indices() works", {

  f <- swmmr:::remove_at_indices
  
  expect_error(f())

  x <- 1:5
  expect_identical(f(x, 3), setdiff(x, 3))
  expect_identical(f(x, integer()), x)
  expect_identical(f(x, which(x > 5L)), x)
  
})
