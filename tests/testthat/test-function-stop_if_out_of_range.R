test_that("stop_if_out_of_range() works", {

  f <- swmmr:::stop_if_out_of_range
  
  expect_error(f())

  expect_silent(f(2, 1, 3))
  expect_error(f(4, 1, 3))
  
})
