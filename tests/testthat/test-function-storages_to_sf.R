#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("storages_to_sf() works", {

  f <- swmmr:::storages_to_sf
  
  expect_error(f())

  x <- init_inp_list()

  expect_warning(f(x), "missing")

  x$storage <- dirty_data_frame(
    Name = "ab"
  )
  
  x$coordinates <- dirty_data_frame(
    Node = c("a", "b"),
    `X-Coord` = 1:2,
    `Y-Coord` = 2:3
  )
  
  expect_inherits_sf(f(x))
  
})
