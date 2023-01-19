test_that("write_inp() works", {

  f <- swmmr:::write_inp
  
  expect_error(f())

  x <- init_inp_list()
  
  file <- tempfile()
  
  f(x, file = file)
  
  expect_true(file.exists(file))
})
