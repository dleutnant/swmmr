#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.polygons() works", {

  f <- swmmr:::parse_section.polygons
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  #cat(paste0('"', names(result), '"', collapse = ",\n"))
  
  expect_data_frame(result, 1L, names = c(
    "Subcatchment",
    "X-Coord",
    "Y-Coord"
  ))
  
})
