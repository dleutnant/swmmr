test_that("read_out() works", {

  f <- swmmr:::read_out
  
  expect_warning(capture.output(result <- f()))

  expect_identical(result, list(error = 1L))
})
