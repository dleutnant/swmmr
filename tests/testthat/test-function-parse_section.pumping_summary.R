#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.pumping_summary() works", {

  f <- swmmr:::parse_section.pumping_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 6L, names = c(
    "Pump",
    "Percent_Utilized",
    "Number_of_Start_Ups",
    "Min_Flow",
    "Avg_Flow",
    "Max_Flow",
    "Total_Volume",
    "Power_Usage",
    "Time_Off_Pump_Curve_Low",
    "Time_Off_Pump_Curve_High"
  ))
  
})
