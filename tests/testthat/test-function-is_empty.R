#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("is_empty() works", {

  f <- swmmr:::is_empty
  
  expect_error(f())

  expect_true(!any(f(1:10)))
  
  expect_identical(f(c(1, NA, 2)), c(FALSE, TRUE, FALSE))
  expect_identical(f(c(1, 2, "")), c(FALSE, FALSE, TRUE))
  expect_identical(f(c("\t", 2, 3)), c(TRUE, FALSE, FALSE))
  expect_identical(f(c("\t", "", "  ")), c(TRUE, TRUE, TRUE))
  
})
