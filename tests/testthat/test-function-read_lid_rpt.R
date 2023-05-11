#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("read_lid_rpt() works", {

  f <- swmmr:::read_lid_rpt
  
  expect_error(f())

  # TODO: add test  
})
