#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.aquifers() works", {

  f <- swmmr:::parse_section.aquifers
  
  expect_error(f())

  # TODO: add reasonable tests
  
  expect_data_frame(f(data.frame(value = 1)))
})
