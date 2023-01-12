#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.groundwater_summary() works", {

  f <- swmmr:::parse_section.groundwater_summary  
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = 2)
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 7L, names = c(
    "Subcatchment", 
    "Total_Infil", 
    "Total_Evap", 
    "Total_Lower_Seepage",  
    "Total_Lateral_Outflow", 
    "Maximum_Lateral_Outflow", 
    "Average_Upper_Moist",  
    "Average_Water_Table", 
    "Final_Upper_Moist", 
    "Final_Water_Table"
  ))
  
})
