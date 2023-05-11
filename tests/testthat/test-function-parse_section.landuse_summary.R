#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.landuse_summary() works", {

  f <- swmmr:::parse_section.landuse_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:7, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 5L, names = c(
    "Name", 
    "Sweeping_Interval", 
    "Maximum_Removal", 
    "Last_Swept"
  ))
  
})
