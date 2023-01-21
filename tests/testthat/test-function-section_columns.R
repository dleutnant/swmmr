#library(testthat)

test_that("section_columns() works", {

  f <- swmmr:::section_columns
  
  result <- f()

  expect_type(result, "list")
  expect_true(all(lengths(result) > 0L))
  
  #swmmr:::input_sections %in% names(result)
})
