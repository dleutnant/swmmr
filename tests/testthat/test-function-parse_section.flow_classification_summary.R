#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.flow_classification_summary() works", {

  f <- swmmr:::parse_section.flow_classification_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, nrow = nrow(x) - 5L, names = c(
    "Conduit", "Adjusted_Actual_Length", "Dry", "Up_Dry", "Down_Dry", 
    "Sub_Crit", "Sup_Crit", "Up_Crit", "Down_Crit", "Norm_Ltd",
    "Inlet_Ctrl"
  ))
  
})
