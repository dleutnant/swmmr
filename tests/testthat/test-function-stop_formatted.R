#library(testthat)

test_that("stop_formatted() works", {

  f <- swmmr:::stop_formatted
  
  expect_error(f())

  expect_error(f("hello, %s", "world"), "hello, world")
  expect_error(f("%d errors", 3L), "3 errors")
})
