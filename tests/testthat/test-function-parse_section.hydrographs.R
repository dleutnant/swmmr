#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.hydrographs() works", {

  f <- swmmr:::parse_section.hydrographs
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  #cat(kwb.utils::objectToText(names(result)))
  
  expect_data_frame(result, 1L, names = c(
    "Hydrograph", 
    "Rain Gage/Month", 
    "Response", 
    "R", "T", "K",  "Dmax", "Drecov", "Dinit"
  ))
  
})
