#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.link_flow_summary() works", {

  f <- swmmr:::parse_section.link_flow_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:7, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 6L, names = c(
    "Link", "Type", "Maximum_Flow", 
    "Time_of_Max_Occurance_d",  "Time_of_Max_Occurance_hm", 
    "Maximum_Veloc", "Maximum_Full_Flow",  "Maximum_Full_Depth"
  ))
  
})
