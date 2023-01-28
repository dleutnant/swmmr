#library(testthat)
#source("tests/testthat/helpers.R")

test_that("run_swmm() works", {

  skip_on_ci()
  
  f <- swmmr:::run_swmm

  expect_error(f())
  
  inp_file <- swmmr:::example_input_files(1L)
  
  result <- f(inp_file, stdout = FALSE)

  expect_true(all(sapply(result, file.exists)))
})
