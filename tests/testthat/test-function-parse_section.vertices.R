#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.vertices() works", {

  f <- swmmr:::parse_section.vertices
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, c("Link", "X-Coord", "Y-Coord"))
  
})
