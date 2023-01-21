#library(testthat)
#source("tests/testthat/helpers_general.R")

test_that("read_list_of_sections() works", {

  f <- swmmr:::read_list_of_sections
  
  expect_error(f())

  path_options <- tempfile(fileext = ".txt")
  
  writeLines(con = path_options, c(
    "[options]",
    "a\t1",
    "b\t2"
  ))
  
  result <- f(path_options)
  
  expect_identical(
    result$options, 
    tibble::tibble(a = 1L, b = 2L)
  )
})
