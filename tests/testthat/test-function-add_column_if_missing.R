test_that("add_column_if_missing() works", {

  f <- swmmr:::add_column_if_missing
  
  expect_error(f())

})
