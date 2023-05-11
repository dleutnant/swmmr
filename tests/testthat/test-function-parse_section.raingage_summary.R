#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.raingage_summary() works", {

  f <- swmmr:::parse_section.raingage_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  result <- f(x)

  expect_data_frame(result, nrow(x) - 5L, names = c(
    "Name", "Data_Source", "Data_Type", "Recording_Interval"
  ))
  
})
