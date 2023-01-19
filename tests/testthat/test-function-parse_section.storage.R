#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.storage() works", {

  f <- swmmr:::parse_section.storage
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Name", "Elev.", "MaxDepth", "InitDepth", "Shape", "Curve Name/Params", 
    "N/A", "Fevap", "Psi", "Ksat", "IMD"    
  ))
  
})
