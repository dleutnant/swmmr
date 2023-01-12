#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.dwf() works", {

  f <- swmmr:::parse_section.dwf
  
  expect_error(f())

  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result)
  expect_nrow(result, 1L)
  expect_names(result, c("Node", "Constituent", "Baseline", "Patterns"))
  
})
