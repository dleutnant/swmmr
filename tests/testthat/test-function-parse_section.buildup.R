#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.buildup() works", {

  f <- swmmr:::parse_section.buildup
  
  expect_error(f())

  x <- data.frame(value = 1:3)
  
  expect_names(f(x), c(
    "LandUse", 
    "Pollutant", 
    "Function", 
    "Coeff1", 
    "Coeff2", 
    "Coeff3", 
    "Per_Unit"
  ))
  
})
