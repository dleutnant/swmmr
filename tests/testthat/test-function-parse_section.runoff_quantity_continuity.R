#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.runoff_quantity_continuity() works", {

  f <- swmmr:::parse_section.runoff_quantity_continuity
  
  expect_error(f())
  
  x <- data.frame(value = 1:10, name = "a")
  
  result <- f(x)
  
  # TODO: try to understand  
  expect_data_frame(result, nrow(x) - 2L, names = c(
    "Component", "Volume", "Depth"
  ))
  
})
