#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("in_brackets() works", {

  f <- swmmr:::in_brackets
  
  expect_error(f())

  expect_identical(f("a"), "[a]")
  expect_identical(f(c("a", "b")), c("[a]", "[b]"))
  
})
