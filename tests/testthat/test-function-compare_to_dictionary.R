test_that("compare_to_dictionary() works", {

  f <- swmmr:::compare_to_dictionary
  
  expect_silent(f())

  x <- data.frame(a = 1:3)
  
  expect_identical(f(shp = x), x)
  expect_identical(f(sf = x), x)
})
