#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.lid_performance_summary() works", {

  f <- swmmr:::parse_section.lid_performance_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:7, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 6L, names = c(
    "Subcatchment", "LID Control", "Total_Inflow", 
    "Evap_Loss",  "Infil_Loss", 
    "Surface_Outflow", "Drain_Outflow", 
    "Initial_Storage",  "Final_Storage", 
    "Continuity_Error"
  ))
  
})
