#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.time_step_critical_elements() works", {

  f <- swmmr:::parse_section.time_step_critical_elements
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 3L, c(
    "Component", "Name", "Value"
  ))
})
