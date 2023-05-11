#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.infiltration() works", {

  f <- swmmr:::parse_section.infiltration
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x, "model")
  
  expect_data_frame(result, 1L, names = c(
    "Subcatchment"
  ))
  
})
