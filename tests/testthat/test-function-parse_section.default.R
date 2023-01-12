#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.default() works", {

  f <- swmmr:::parse_section.default
  
  expect_error(f())

  x <- data.frame()
  
  expect_warning(f(x), "Unknown class: data.frame()")
})
