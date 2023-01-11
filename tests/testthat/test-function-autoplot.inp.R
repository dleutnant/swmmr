test_that("autoplot.inp() works", {

  f <- swmmr:::autoplot.inp
  
  expect_error(f())

  x <- swmmr:::read_example_input_files(ids = 1L)[[1L]]
  class(x)

  suppressWarnings(result <- f(x))

  inherits(result, "ggplot")
})
