#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.lid_usage() works", {

  f <- swmmr:::parse_section.lid_usage
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Subcatchment", "LID Process", "Number", "Area", "Width", "InitSat",  
    "FromImp", "ToPerv", "RptFile", "DrainTo"
  ))
  
})
