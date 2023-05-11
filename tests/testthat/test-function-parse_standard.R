#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("parse_standard() works", {

  f <- swmmr:::parse_standard
  
  expect_error(f())

  x <- data.frame(value = 1:3)
  
  result <- f(x, "evaporation")

  expect_identical(names(result), c("Data Source", "Parameters"))
    
})
