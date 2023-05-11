#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("comma_space_collapsed() works", {

  f <- swmmr:::comma_space_collapsed
  
  expect_error(f())

  expect_identical(f(letters[1:3]) , "a, b, c")
  
})
