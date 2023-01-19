#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.tags() works", {

  f <- swmmr:::parse_section.tags
  
  expect_error(f())
  
  x <- data.frame(value = 1)
  
  result <- f(x)
  
  expect_data_frame(result, 1L, c("object", "id", "text"))
})
