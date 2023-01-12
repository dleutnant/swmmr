#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("list_of_sections_to_inp() works", {

  f <- swmmr:::list_of_sections_to_inp
  
  expect_error(f())

  list_of_sections <- list()
  
  expect_inherits(f(list_of_sections), "inp")
})
