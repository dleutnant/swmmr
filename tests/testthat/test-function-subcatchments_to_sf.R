#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("subcatchments_to_sf() works", {

  f <- swmmr:::subcatchments_to_sf
  
  expect_error(f())

  x <- list()
  class(x) <- "inp"
  
  expect_warning(f(x), "incomplete")

  x$subcatchments <- data.frame(Name = "a")
  x$subareas <- data.frame(Subcatchment = "a")
  x$infiltration <- data.frame(Subcatchment = "a")
  x$polygons <- dirty_data_frame(
    Subcatchment = "a",
    `X-Coord` = 1,
    `Y-Coord` = 2
  )

  result <- f(x)
  expect_inherits_sf(result)
})
