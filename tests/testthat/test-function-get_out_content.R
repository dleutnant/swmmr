test_that("get_out_content() works", {

  f <- swmmr:::get_out_content
  
  expect_warning(result <- f())

  expect_identical(result, list(error = 1L))
})
