test_that("autoplot.inp() works", {

  f <- swmmr:::autoplot.inp
  
  expect_error(f())

  x <- swmmr:::read_example_input_files(ids = 1L)[[1L]]
  
  expect_s3_class(x, "inp")

  suppressWarnings(result <- f(x))

  expect_s3_class(result, "ggplot")
  
})
