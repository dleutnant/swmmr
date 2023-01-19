test_that("check_package_and_class() works", {

  f <- swmmr:::check_package_and_class
  
  expect_error(f())

  x <- 1
  class(x) <- "inp"
  
  expect_silent(f(x))
})
