#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("read_rpt() works", {

  f <- swmmr:::read_rpt
  
  expect_error(f())

  expect_error(f(tempfile()), "does not exist")
  
  file <- tempfile()
  
  writeLines("hallo", file)
  
  expect_message(f(file), "There are errors")
})
