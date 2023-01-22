#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("get_section_names_for_input() works", {

  f <- swmmr:::get_section_names_for_input
  
  result <- f()
  
  expect_type(result, "character")

  expect_identical(result[1:3], c("title", "options", "evaporation"))
  
})
