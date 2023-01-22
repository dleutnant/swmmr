#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("warn_formatted() works", {

  f <- swmmr:::warn_formatted
  
  expect_error(f())

  expect_warning(f("abc"), "abc")
  expect_warning(f("a: %d", 1L), "a: 1")
})
