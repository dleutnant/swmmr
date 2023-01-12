test_that("GetSwmmResult() works", {

  f <- swmmr:::GetSwmmResult
  
  expect_error(f())
  
  expect_output(f(iType = 0L, vIndex = 0L, iIndex = 0L))
})

