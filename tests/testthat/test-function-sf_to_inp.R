#library(testthat)
#source("tests/testthat/helpers_general.R")
test_that("sf_to_inp() works", {

  f <- swmmr:::sf_to_inp
  
  suppressWarnings(expect_warning(result <- f()))

  expect_inherits(result, "inp")
  
  expected <- c("options", "evaporation", "timeseries", "report")
  expect_true(all(expected %in% names(result)))
})
