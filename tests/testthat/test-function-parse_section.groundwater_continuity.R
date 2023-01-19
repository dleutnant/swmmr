#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.groundwater_continuity() works", {

  f <- swmmr:::parse_section.groundwater_continuity
  
  expect_error(f())
  
  x <- data.frame(value = 1:5, name = 2)
  
  result <- f(x)
  
  #cat(kwb.utils::objectToText(names(result)))
  
  expect_data_frame(result, nrow(x) - 2L, names = c(
    "Component", "Volume", "Depth"
  ))
  
})
