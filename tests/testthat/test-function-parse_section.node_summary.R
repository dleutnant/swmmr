#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.node_summary() works", {

  f <- swmmr:::parse_section.node_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:6, name = "A")
  
  result <- f(x)
  
  #cat(kwb.utils::objectToText(names(result)))
  
  expect_data_frame(result, nrow(x) - 5L, names = c(
    "Name", "Type", "Invert_Elev", "Max_Depth", "Ponded_Area",  
    "External_Inflow"
  ))
  
})
