#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("set_class() works", {

  f <- swmmr:::set_class
  
  expect_error(f())

  expect_s3_class(f(1, "a"), "a")
  expect_s3_class(f(data.frame(), "my-class"), "my-class")
  
})
