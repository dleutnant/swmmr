#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("orifices_to_sf() works", {

  f <- swmmr:::orifices_to_sf
  
  expect_error(f())

  x <- init_inp_list(orifices = dirty_data_frame(
    Name = "ab",
    `From Node` = "a",
    `To Node` = "b"
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
