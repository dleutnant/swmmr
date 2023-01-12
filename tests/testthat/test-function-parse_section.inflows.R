#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.inflows() works", {

  f <- swmmr:::parse_section.inflows
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  #cat(kwb.utils::objectToText(names(result)))
  
  expect_data_frame(result, 1L, names = c(
    "Node", 
    "Constituent", 
    "Time Series", 
    "Type", 
    "Mfactor", 
    "Sfactor",  
    "BaseLine", 
    "Pattern"
  ))
  
})
