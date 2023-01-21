#library(testthat)

test_that("extract_sections() works", {

  f <- swmmr:::extract_sections
  
  expect_error(f())

  x <- c(
    "[a]",
    "",
    "a1",
    "[b]",
    "b1",
    "b2"
  )
  
  result <- f(x)

  expect_identical(result, list(
    a = c("", "a1"), 
    b = c("b1", "b2")
  ))
    
})
