#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.storage_volume_summary() works", {

  f <- swmmr:::parse_section.storage_volume_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 6L, names = c(
    "Storage_Unit", "Average_Volume", "Avg_Pcnt_Full", "Evap_Pcnt_Loss", 
    "Exfil_Pcnt_Loss", "Maximum_Volume", "Max_Pcnt_Full", 
    "Time_of_Max_Occurence_days", "Time_of_Max_Occurence_hr_min", 
    "Maximum_Outflow"
  ))
  
})
