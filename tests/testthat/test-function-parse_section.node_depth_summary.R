#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.node_depth_summary() works", {

  f <- swmmr:::parse_section.node_depth_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:8, name = "A")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 6L, names = c(
    "Node", "Type", "Average_Depth", "Maximum_Depth", "Maximum_HGL",  
    "Time_of_Max_Occurance_d", "Time_of_Max_Occurance_hm", 
    "Reported_Max_Depth"
  ))
  
})
