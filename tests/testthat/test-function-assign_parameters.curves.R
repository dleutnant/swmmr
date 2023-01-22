#library(testthat)
test_that("assign_parameters.curves() works", {

  f <- swmmr:::assign_parameters.curves
  
  expect_error(f())

  x <- read.csv(text = c(
    "Name,Type,X,Y",
    "a,Pump1,1,2",
    "a,Pump1,3,4",
    "b,Pump2,5,6",
    "b,Pump2,7,8",
    "c,Pump3,7,8"
  ))
  
  result <- f(x)
  
  expect_identical(result$Type, c("Pump1", " ", "Pump2", " ", "Pump3"))
  
})
