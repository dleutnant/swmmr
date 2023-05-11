#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.link_pollutant_load_summary() works", {

  f <- swmmr:::parse_section.link_pollutant_load_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:7, name = "a")
  
  result <- f(x)
  
  # TODO: check what should happen
  expect_data_frame(result, nrow(x) - 5L, names = c(
    "Link", "4", "a"
  ))
  
})
