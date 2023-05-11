#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.element_count() works", {

  f <- swmmr:::parse_section.element_count
  
  expect_error(f())
  
  indices <- 1:5
  x <- data.frame(name = paste("name", indices), value = indices)
  
  result <- f(x)
  
  expect_data_frame(result)
  expect_nrow(result, length(indices) - 3L)
  
  #cat(kwb.utils::objectToText(names(result)))
  
  expect_names(result, c("Element", "Count"))
  
})
