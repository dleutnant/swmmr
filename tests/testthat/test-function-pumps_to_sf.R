#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("pumps_to_sf() works", {
  
  f <- swmmr:::pumps_to_sf
  
  expect_error(f())
  
  x <- init_inp_list()
  
  expect_warning(result <- f(x), "missing: pumps, coordinates")
  
  x$pumps <- dirty_data_frame(
    Name = "pump",
    `From Node` = "a",
    `To Node` = "b"
  )
  
  x$coordinates <- dirty_data_frame(
    Node = c("a", "b"),
    `X-Coord` = 1:2,
    `Y-Coord` = 2:3
  )
  
  result <- f(x)
  
  expect_inherits_sf(result)
})
