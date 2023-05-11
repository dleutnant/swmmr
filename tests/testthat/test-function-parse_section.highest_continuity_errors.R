#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.highest_continuity_errors() works", {

  f <- swmmr:::parse_section.highest_continuity_errors
  
  expect_error(f())
  
  x <- data.frame(value = 1:5, name = 2:6)
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 3L, names = c(
    "Component", "Name", "Error"
  ))

})
