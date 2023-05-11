#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.lid_control_summary() works", {

  f <- swmmr:::parse_section.lid_control_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:6, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Subcatchment", 
    "LID_Control", 
    "No_of_Units", 
    "Unit_Area",  
    "Unit_Width", 
    "Percent_Area_Covered", 
    "Percent_Imperv_Treated",  
    "Percent_Treated"
  ))
  
})
