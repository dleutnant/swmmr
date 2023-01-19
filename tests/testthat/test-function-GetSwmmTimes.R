test_that("GetSwmmTimes() works", {

  skip_on_ci()
  
  f <- swmmr:::GetSwmmTimes
  
  expect_length(f(), 0L)

})
