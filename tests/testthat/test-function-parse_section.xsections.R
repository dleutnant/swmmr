#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.xsections() works", {
  
  f <- swmmr:::parse_section.xsections
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, c(
    "Link", "Shape", "Geom1", "Geom2", "Geom3", "Geom4", "Barrels", "Culvert"
  ))
  
})
