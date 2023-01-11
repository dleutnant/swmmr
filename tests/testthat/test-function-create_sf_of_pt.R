test_that("create_sf_of_pt() works", {

  f <- swmmr:::create_sf_of_pt
  
  expect_error(f())

  x <- data.frame(
    name = LETTERS[1:10],
    `X-Coord` = 1:10, 
    `Y-Coord` = 2:11,
    check.names = FALSE
  )
  
  expect_true(inherits(f(x), "sf"))
})
