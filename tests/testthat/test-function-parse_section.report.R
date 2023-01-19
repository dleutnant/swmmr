#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.report() works", {

  f <- swmmr:::parse_section.report
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Reporting Options"
  ))
  
})
