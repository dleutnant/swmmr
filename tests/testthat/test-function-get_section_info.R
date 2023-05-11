#library(testthat)

test_that("get_section_info() works", {

  f <- swmmr:::get_section_info
  
  expect_error(f())
  
  result <- f(c(
    "[a]", #1
    "a1",  #2
    "a2",  #3
    "",    #4
    "[b]", #5
    "b1"   #6
  ))

  expect_identical(result$name, c("a", "b"))
  expect_identical(result$start, c(2L, 6L))
  expect_identical(result$end, c(3L, 6L))
})
