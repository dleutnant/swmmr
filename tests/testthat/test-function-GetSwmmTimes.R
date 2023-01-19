test_that("GetSwmmTimes() works", {

  f <- swmmr:::GetSwmmTimes
  
  expect_length(f(), 0L)

})
