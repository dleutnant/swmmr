#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("links_to_sf() works", {

  f <- swmmr:::links_to_sf
  
  expect_error(f())

  x <- list(conduits = dirty_data_frame(
    Name = "ab",
    `From Node` = "a", 
    `To Node` = "b"
  ))
  
  class(x) <- "inp"
  
  expect_warning(f(x), "incomplete features")

  x$coordinates <- dirty_data_frame(
    Node = c("a", "b"),
    `X-Coord` = 1:2,
    `Y-Coord` = 2:3
  )
  
  result <- f(x)

  expect_inherits_sf(result)
})
