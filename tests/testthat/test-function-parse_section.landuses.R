#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.landuses() works", {

  f <- swmmr:::parse_section.landuses
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Name", "Sweeping_Interval", "Fraction_Available", "Last_Swept" 
  ))
  
})
