test_that("which_last() works", {

  f <- swmmr:::which_last
  
  expect_error(f())

  expect_identical(f(c(TRUE, FALSE, TRUE)), 3L)
  expect_identical(f(c(FALSE, TRUE, TRUE)), 2:3)
  expect_identical(f(c(TRUE, TRUE, TRUE)), 1:3)
  expect_identical(f(c(TRUE, TRUE, FALSE)), integer())
  
})
