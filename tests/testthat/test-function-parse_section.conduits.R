#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.conduits() works", {

  f <- swmmr:::parse_section.conduits
  
  expect_error(f())

  x <- data.frame(value = 1:3)
  
  expect_warning(f(x), "9 pieces")
})
