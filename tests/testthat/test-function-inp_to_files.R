test_that("inp_to_files() works", {

  f <- swmmr:::inp_to_files
  
  expect_error(f())

  x <- list()
  
  expect_error(f(x))
  
  class(x) <- "inp"

  expect_silent(f(x, name = "name", path_out = tempdir(), quiet = TRUE))
})
