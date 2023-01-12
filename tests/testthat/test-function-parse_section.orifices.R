#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.orifices() works", {

  f <- swmmr:::parse_section.orifices
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  #cat(kwb.utils::objectToText(names(result)))
  
  expect_data_frame(result, 1L, names = c(
    "Name", "From Node", "To Node", "Type", "Offset", "Qcoeff",  "Gated", 
    "CloseTime"
  ))
  
})
