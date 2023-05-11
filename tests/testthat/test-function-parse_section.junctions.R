#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.junctions() works", {

  f <- swmmr:::parse_section.junctions
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  expect_warning(result <- f(x), "6 pieces")
  
  expect_data_frame(result, 1L, names = c(
    "Name", 
    "Elevation", 
    "MaxDepth", 
    "InitDepth", 
    "SurDepth", 
    "Aponded"  
  ))
  
})
