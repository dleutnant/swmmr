#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.outfall_loading_summary() works", {

  f <- swmmr:::parse_section.outfall_loading_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  result <- f(x)
  
  #cat(kwb.utils::objectToText(names(result)))
  
  expect_data_frame(result, nrow(x) - 6L, names = c(
    "Outfall_Node", "Flow_Freq", "Avg_Flow", "Max_Flow", "Total_Volume"
  ))
  
})
