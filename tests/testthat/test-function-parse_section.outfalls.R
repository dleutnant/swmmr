#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.outfalls() works", {

  f <- swmmr:::parse_section.outfalls
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, names = c(
    "Name", "Elevation", "tab2", "Type", "tab3", "Stage Data",  
    "tab4", "Gated", "tab5", "Route To"
  ))
  
})
