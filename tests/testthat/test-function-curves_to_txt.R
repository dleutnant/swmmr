test_that("curves_to_txt() works", {

  f <- swmmr:::curves_to_txt
  
  expect_error(f())

  x <- list()
  class(x) <- "inp"
  
  expect_message(f(x), "^section curves is missing")
})
