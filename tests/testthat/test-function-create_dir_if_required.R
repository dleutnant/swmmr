#source("tests/testthat/helpers_general.R")

test_that("create_dir_if_required() works", {

  f <- swmmr:::create_dir_if_required
  
  expect_error(f())

  path <- tempfile()
  
  expect_output(f(path, silent = FALSE))
  expect_silent(f(path))
  
  expect_file_exists(path)
})
