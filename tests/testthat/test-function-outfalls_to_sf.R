#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("outfalls_to_sf() works", {

  f <- swmmr:::outfalls_to_sf
  
  expect_error(f())

  x <- init_inp_list(outfalls = dirty_data_frame(
    Name = "ab"
  ))
  
  expect_warning(f(x), "missing: coordinates")

  x$coordinates <- dirty_data_frame(
    Node = c("a", "b"),
    `X-Coord` = 1:2,
    `Y-Coord` = 2:3
  )
  
  result <- f(x)
  
  expect_inherits_sf(result)
})
