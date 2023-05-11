#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.evaporation() works", {

  f <- swmmr:::parse_section.evaporation

  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result)
  expect_nrow(result, 1L)
  expect_names(result, c("Data Source", "Parameters"))
  
})
