test_that("assign_parameters.xsections() works", {

  f <- swmmr:::assign_parameters.xsections
  
  expect_error(f())

  result <- f(data.frame(Name = "a"))
  
  expected_names <- names(swmmr:::get_column_defaults()$xsections)
  
  expect_true(all(expected_names %in% names(result)))
  
})
