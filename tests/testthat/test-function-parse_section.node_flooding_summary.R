#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.node_flooding_summary() works", {

  f <- swmmr:::parse_section.node_flooding_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "A")
  
  result <- f(x)
  
  #cat(kwb.utils::objectToText(names(result)))
  
  expect_data_frame(result, nrow(x) - 8L, names = c(
    "Node", "Hours_Flooded", "Maximum_Rate", 
    "Time_of_Max_Occurance_d", "Time_of_Max_Occurance_hm", 
    "Total_Flood_Volume", "Maximum_Ponded_Volume"
  ))
  
})
