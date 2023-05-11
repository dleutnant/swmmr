#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.pollutant_summary() works", {

  f <- swmmr:::parse_section.pollutant_summary
  
  expect_error(f())
  
  x <- data.frame(value = 1:6, name = "a")
  
  result <- f(x)
  
  expect_data_frame(result, nrow(x) - 5L, names = c(
    "Name",
    "Units",
    "Ppt_Concen",
    "GW_Concen",
    "Kdecay_per_day",
    "CoPollutant"
  ))
  
})

