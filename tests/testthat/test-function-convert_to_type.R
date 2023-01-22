#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("convert_to_type() works", {

  f <- swmmr:::convert_to_type
  
  expect_error(f())

  expect_identical(f("1", "integer"), 1L)
  expect_identical(f("1", "integer"), 1L)
  expect_identical(f(1L, "character"), "1")
  expect_identical(f("2023-01-22", "Date"), as.Date("2023-01-22"))
  
})
