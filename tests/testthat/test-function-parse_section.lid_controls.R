#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.lid_controls() works", {

  f <- swmmr:::parse_section.lid_controls
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Name", "Type/Layer", 
    "Par1", "Par2", "Par3", "Par4", "Par5",  "Par6", "Par7"
  ))
  
})
