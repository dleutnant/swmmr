#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.map() works", {

  f <- swmmr:::parse_section.map
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  expect_warning(result <- f(x), "Expected 2 pieces")
  
  expect_data_frame(result, 1L, names = c(
    "key", "value"
  ))
  
})
