test_that("separate_into() works", {

  f <- swmmr:::separate_into
  
  expect_error(f())

  x <- data.frame(
    value = c("a 1", "a 2", "b 1", "b 2")
  )
  
  expect_identical(
    f(x, c("key", "value")), 
    data.frame(
      key = c("a", "a", "b", "b"), 
      value = c(1L, 2L, 1L, 2L)
    )
  )
  
})
