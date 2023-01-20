test_that("replace_values() works", {

  f <- swmmr:::replace_values
  
  expect_error(f())

  result <- f(letters[1:5], from = c("b", "d"), to = c("B", "D"))
  
  expect_identical(result, c("a", "B", "c", "D", "e"))
})
