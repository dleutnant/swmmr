#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.routing_time_step_summary() works", {

  f <- swmmr:::parse_section.routing_time_step_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 3L, names = c(
    "Component", "Value"
  ))
  
})
