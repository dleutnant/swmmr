test_that("which_marginal() works", {

  f <- swmmr:::which_marginal
  
  expect_error(f())

  expect_identical(f(c(TRUE, FALSE), first = TRUE), 1L)
  expect_identical(f(c(TRUE, FALSE, TRUE), first = FALSE), 3L)
  
})
