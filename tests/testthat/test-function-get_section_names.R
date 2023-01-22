#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("get_section_names() works", {

  f <- swmmr:::get_section_names
  
  expect_error(f())

  expect_true("aquifers" %in% f("input"))
  expect_true("Storage Volume Summary" %in% f("report"))
  expect_length(f("what-is-that"), 0L)
  
})
