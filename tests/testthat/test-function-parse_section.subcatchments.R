#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.subcatchments() works", {

  f <- swmmr:::parse_section.subcatchments
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  expect_warning(result <- f(x), "9 pieces")

})
