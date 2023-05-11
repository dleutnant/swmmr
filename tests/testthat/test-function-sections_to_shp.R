test_that("sections_to_shp() works", {

  f <- swmmr:::sections_to_shp
  
  expect_error(f())

  x <- 1
  class(x) <- "inp"
  
  path_out <- tempdir()
  
  f(x, path_out = path_out, quiet = TRUE)

  expect_true("shp" %in% dir(path_out))

})
