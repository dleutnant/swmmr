#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.timeseries() works", {

  f <- swmmr:::parse_section.timeseries
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, c(
    "Name", "Date", "Time", "Value"
  ))
})
