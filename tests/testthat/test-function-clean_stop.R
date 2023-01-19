test_that("clean_stop() works", {

  f <- swmmr:::clean_stop
  
  expect_error(f())

  expect_error(f("error"))
})
