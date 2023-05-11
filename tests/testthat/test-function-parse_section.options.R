#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.options() works", {

  f <- swmmr:::parse_section.options
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Option", "Value"
  ))
  
})
