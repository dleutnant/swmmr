#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.quality_routing_continuity() works", {

  f <- swmmr:::parse_section.quality_routing_continuity
  
  expect_error(f())
  
  indices <- 1:4
  x <- data.frame(value = indices, name = paste0("a", indices))
  
  result <- f(x)
  
  # TODO: check what should happen
  expect_data_frame(result, nrow(x) - 2L, names = c(
    "Component",
    "1",
    "a1"
  ))
  
})
