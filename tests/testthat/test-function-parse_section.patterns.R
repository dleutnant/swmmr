#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.patterns() works", {

  f <- swmmr:::parse_section.patterns
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Name",
    "Type",
    "Multipliers"
  ))
  
})
