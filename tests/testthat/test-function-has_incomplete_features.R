test_that("has_incomplete_features() works", {

  f <- swmmr:::has_incomplete_features
  
  expect_error(f())

  x <- data.frame(x = 1)
  
  expect_warning(
    result <- f(x, "subject", "a"), 
    "incomplete features: subject"
  )
  
  expect_true(result)
})
