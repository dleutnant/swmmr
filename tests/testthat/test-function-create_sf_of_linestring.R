#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("create_sf_of_linestring() works", {

  f <- swmmr:::create_sf_of_linestring
  
  expect_error(f())

  x <- dirty_data_frame(
    Name = LETTERS[1:3],
    pos = 1:3, 
    id = 11:13, 
    `X-Coord` = 1:3,
    `Y-Coord` = 11:13
  )
 
  result <- f(x)
  
  expect_inherits_sf(result)
})
