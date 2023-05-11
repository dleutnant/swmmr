#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.groundwater() works", {

  f <- swmmr:::parse_section.groundwater
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Subcatchment", 
    "Aquifer", 
    "Node", 
    "Esurf", 
    "A1", "B1", "A2", "B2", "A3", "Dsw", "Egwt", "Ebot", "Wgr", "Umc"
  ))
  
})
