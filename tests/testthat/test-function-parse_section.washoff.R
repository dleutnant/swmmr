#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.washoff() works", {

  f <- swmmr:::parse_section.washoff
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, c(
    "LandUse", "Pollutant", "Function", "Coeff1", "Coeff2", "SweepRmvl",
    "BmpRmvl"
  ))
  
})
