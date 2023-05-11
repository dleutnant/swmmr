#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("raingages_to_sf() works", {
  
  f <- swmmr:::raingages_to_sf
  
  expect_error(f())
  
  x <- init_inp_list(Name = data.frame())
  
  expect_warning(f(x), "missing: raingages")
  
  x$raingages <- dirty_data_frame(
    Name = "a",
    `X-Coord` = 1,
    `Y-Coord` = 2
  )
  
  x$symbols <- data.frame(Gage = "a")
  
  result <- f(x)
  
  expect_inherits_sf(result)
})
