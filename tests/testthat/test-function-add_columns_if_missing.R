test_that("add_columns_if_missing() works", {

  f <- swmmr:::add_columns_if_missing
  
  expect_error(f())

})
