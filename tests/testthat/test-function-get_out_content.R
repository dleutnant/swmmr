test_that("get_out_content() works", {

  f <- swmmr:::get_out_content
  
  expect_warning(capture.output(result <- f()))

  expect_identical(result, list(error = 1L))
})
