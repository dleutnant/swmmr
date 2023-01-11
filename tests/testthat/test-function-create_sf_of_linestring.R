test_that("create_sf_of_linestring() works", {

  f <- swmmr:::create_sf_of_linestring
  
  expect_error(f())

  x <- data.frame(
    Name = LETTERS[1:3],
    pos = 1:3, 
    id = 11:13, 
    `X-Coord` = 1:3,
    `Y-Coord` = 11:13,
    check.names = FALSE
  )
 
  result <- f(x)
  
  expect_true(inherits(result, "sf"))
})
