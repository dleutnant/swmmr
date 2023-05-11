test_that("skip_head() works", {

  f <- swmmr:::skip_head
  
  expect_error(f())

  x <- data.frame(a = 1:5)
  
  expect_identical(f(x, n = 1L), x[2:5, ])
  expect_identical(f(x, n = 2L), x[3:5, ])
  
})
