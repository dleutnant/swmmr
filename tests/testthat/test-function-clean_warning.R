test_that("clean_warning() works", {

  f <- swmmr:::clean_warning
  
  expect_warning(f("hello"), "hello")

})
