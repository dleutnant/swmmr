#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.subcatchment_washoff_summary() works", {

  f <- swmmr:::parse_section.subcatchment_washoff_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  result <- f(x)
  
  # TODO: check this out
  expect_data_frame(result, nrow(x) - 5L, names = c(
    "Subcatchment", "4", "a"
  ))
  
})
