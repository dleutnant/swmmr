#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.coordinates() works", {

  f <- swmmr:::parse_section.coordinates
  
  expect_error(f())

  x <- data.frame(value = 1)
  
  expect_data_frame(f(x))
})
