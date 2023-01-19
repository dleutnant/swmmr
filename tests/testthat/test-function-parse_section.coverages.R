#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.coverages() works", {

  f <- swmmr:::parse_section.coverages
  
  expect_error(f())

  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, nrow = 1L, names = c(
    "Subcatchment", "Land Use", "Percent"
  ))
  
})
