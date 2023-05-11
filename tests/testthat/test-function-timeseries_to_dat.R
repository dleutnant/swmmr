test_that("timeseries_to_dat() works", {

  f <- swmmr:::timeseries_to_dat
  
  expect_error(f())

  x <- init_inp_list()
  
  expect_message(f(x), "timeseries is missing")
  
  x$timeseries <- data.frame()
  
  path_out <- tempdir()
  
  expect_message(result <- f(x, path_out = path_out), "written to")
  
  expect_true("dat" %in% dir(tempdir()))
})
