#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("junctions_to_sf() works", {

  f <- swmmr:::junctions_to_sf
  
  expect_error(f())

  x <- list(junctions = data.frame(
    Name = "a"
  ))
  
  class(x) <- "inp"
  
  expect_warning(f(x), "incomplete features")
  
  x$coordinates <- dirty_data_frame(
    Node = "a", 
    `X-Coord` = 1, 
    `Y-Coord` = 2
  )

  result <- f(x)
  
  expect_inherits_sf(result)
})
