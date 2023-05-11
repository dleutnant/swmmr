#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.node_inflow_summary() works", {

  f <- swmmr:::parse_section.node_inflow_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "A")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 7L, names = c(
    "Node", "Type", 
    "Maximum_Lateral_Inflow", "Maximum_Total_Inflow", 
    "Time_of_Max_Occurance_d", "Time_of_Max_Occurance_hm", 
    "Lateral_Inflow_Volume",  "Total_Inflow_Volume", "Flow_Balance_Error"
  ))
  
})
