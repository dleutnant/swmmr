#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("read_inp() works", {

  f <- swmmr:::read_inp
  
  expect_error(f())

  inp_file <- swmmr:::example_input_files(1L)[[1L]]
  
  expect_inherits(f(inp_file), "inp")
  
})
