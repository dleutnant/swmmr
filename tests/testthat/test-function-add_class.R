#library(testthat)
#source("tests/testthat/helpers_general.R")
test_that("add_class() works", {

  f <- swmmr:::add_class
  
  expect_error(f())

  expect_inherits(f(1, "a"), "a")
  
})
