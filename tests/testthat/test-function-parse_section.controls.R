#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.controls() works", {

  f <- swmmr:::parse_section.controls
  
  expect_error(f())

  x <- data.frame()
  
  expect_data_frame(f(x))
})
