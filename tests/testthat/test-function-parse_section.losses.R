#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.losses() works", {

  f <- swmmr:::parse_section.losses
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Link", "Kentry", "Kexit", "Kavg", "Flap Gate", "Seepage"
  ))
  
})
