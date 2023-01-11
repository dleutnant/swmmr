test_that("assign_parameters.default() works", {

  f <- swmmr:::assign_parameters.default
  
  expect_error(f())

})
