#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.snowpacks() works", {

  f <- swmmr:::parse_section.snowpacks
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  # TODO: try to understand  
  expect_data_frame(result, 1L, names = c(
    "Name", "Surface", "Parameters"
  ))
  
})
