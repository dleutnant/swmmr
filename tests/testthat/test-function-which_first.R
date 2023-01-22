test_that("which_first() works", {

  f <- swmmr:::which_first
  
  expect_error(f())
  expect_error(f(letters))
  
  expect_identical(f(letters %in% c("a", "b", "c")), 1:3)
  expect_identical(f(letters %in% c("a", "c")), 1L)
  expect_identical(f(c(TRUE, TRUE, FALSE, TRUE)), 1:2)
  
})
