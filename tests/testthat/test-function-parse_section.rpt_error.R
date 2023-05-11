#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.rpt_error() works", {

  f <- swmmr:::parse_section.rpt_error
  
  expect_error(f())
  
  x <- data.frame(value = 1:10)
  
  result <- f(x)

  # TODO: try to understand  
  expect_data_frame(result)
  
})
