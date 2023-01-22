#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("section_info() works", {

  f <- swmmr:::section_info
  
  result <- f()

  expect_identical(names(result), c(
    "type", "section", "key", "n_skip", "input_order", "summary_order"
  ))

  # All "_order" columns should be integer  
  expect_true(all(sapply(result[grepl("order", names(result))], is.integer)))
})
