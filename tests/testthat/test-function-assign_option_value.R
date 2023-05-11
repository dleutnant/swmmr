test_that("assign_option_value() works", {

  f <- swmmr:::assign_option_value
  
  expect_error(f())

  x <- data.frame(a = 1:2, b = 2:3)
  
  result <- f(x)

  expect_inherits(result, "tbl_df")  
  expect_identical(names(result), c("Option", "Value"))
  expect_identical(nrow(result), ncol(x))
})
