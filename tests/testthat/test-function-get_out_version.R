test_that("get_out_version() works", {

  f <- swmmr:::get_out_version
  
  expect_warning(result <- f())

  expect_null(result)
})
