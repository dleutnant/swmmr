#library(testthat)

test_that("assign_parameters.coverages() works", {

  f <- swmmr:::assign_parameters.coverages
  
  expect_error(f())

  x1 <- data.frame(a = 1)
  x2 <- data.frame(Type = 1:2, name = c("a", "b"))
  
  s1 <- data.frame(b = 2)
  s2 <- data.frame(Type = 2, name = "beautiful")
  
  expect_warning(f(x1, subcatchment = s1))
  expect_error(f(x1, subcatchment = s2))
  expect_silent(f(x2, s2))
  
})
