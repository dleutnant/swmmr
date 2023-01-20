test_that("GetSwmmResultPart2() works", {

  skip_on_ci()
  
  f <- swmmr:::GetSwmmResultPart2
  
  expect_error(f())

})
