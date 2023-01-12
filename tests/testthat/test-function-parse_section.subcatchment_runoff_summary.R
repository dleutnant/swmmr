#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.subcatchment_runoff_summary() works", {

  f <- swmmr:::parse_section.subcatchment_runoff_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 6L, names = c(
    "Subcatchment", "Total_Precip", "Total_Runon", "Total_Evap", 
    "Total_Infil", "Total_Runoff_imperv_Depth", "Total_Runoff_perv_Depth",
    "Total_Runoff_Depth", "Total_Runoff_Volume", "Total_Peak_Runoff", 
    "Total_Runoff_Coeff"
  ))
  
})
