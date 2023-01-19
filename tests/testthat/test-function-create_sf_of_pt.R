#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("create_sf_of_pt() works", {

  f <- swmmr:::create_sf_of_pt
  
  expect_error(f())

  x <- dirty_data_frame(
    name = LETTERS[1:10],
    `X-Coord` = 1:10, 
    `Y-Coord` = 2:11
  )
  
  expect_inherits_sf(f(x))
})
