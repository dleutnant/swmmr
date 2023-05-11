#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("trim_vector() works", {

  f <- swmmr:::trim_vector
  
  expect_error(f())

  expect_identical(f(c("", "a", "")), "a")
  expect_identical(f(c("", "a", ""), trim = "head"), c("a", ""))
  expect_identical(f(c("", "a", ""), trim = "head"), c("a", ""))
  expect_error(f(c("", "a", ""), trim = "unknown"))

  expect_identical(f(c(" ", "  ", " a ", "  \t  ")), " a ")

})
