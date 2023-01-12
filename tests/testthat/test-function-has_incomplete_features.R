test_that("has_incomplete_features() works", {

  f <- swmmr:::has_incomplete_features
  
  expect_error(f())

  x <- list(x = 1)
  
  expect_warning(
    result <- f(x, "subject", c("missing_1", "missing_2")), 
    "incomplete features: subject \\(missing: missing_1, missing_2\\)"
  )
  
  expect_true(result)
})
