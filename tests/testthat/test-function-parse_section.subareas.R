#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.subareas() works", {

  f <- swmmr:::parse_section.subareas
  
  expect_error(f())
  
  x <- data.frame(value = 1, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Subcatchment", "N-Imperv", "N-Perv", "S-Imperv", "S-Perv", "PctZero",
    "RouteTo", "PctRouted"
  ))
  
})
