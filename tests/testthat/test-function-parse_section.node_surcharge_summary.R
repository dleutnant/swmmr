#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.node_surcharge_summary() works", {

  f <- swmmr:::parse_section.node_surcharge_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "A")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 7L, names = c(
    "Node", "Type", "Hours_Surcharged", 
    "Max_Height_Above_Crown_Feet",  "Min_Depth_Below_Rim_Feet"
  ))
  
})
