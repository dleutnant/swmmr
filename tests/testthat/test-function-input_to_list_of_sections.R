#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("input_to_list_of_sections() works", {

  f <- swmmr:::input_to_list_of_sections
  
  expect_error(f())

  swmmr::inp_to_files(
    x = swmmr:::read_example_input_files(ids = 1L)[[1L]], 
    name = "my-example", 
    path_out = tempdir()
  )

  path_options <- file.path(tempdir(), "txt/my-example_options.txt")

  expect_warning(result <- f(path_options))
  
})
