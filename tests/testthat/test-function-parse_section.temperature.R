#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.temperature() works", {

  f <- swmmr:::parse_section.temperature
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, c("Data Element", "tab1", "Values"))
})
