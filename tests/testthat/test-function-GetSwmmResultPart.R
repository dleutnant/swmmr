test_that("GetSwmmResultPart() works", {

  f <- swmmr:::GetSwmmResultPart
  
  expect_error(f())

  expect_output(
    f(iType = 0L, iIndex = 0L, vIndex = 0L, firstPeriod = 0L, lastPeriod = 0L)
  )
})
