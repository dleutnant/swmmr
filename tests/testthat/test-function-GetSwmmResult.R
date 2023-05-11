#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("GetSwmmResult() works", {

  skip_on_ci()
  
  f <- swmmr:::GetSwmmResult
  
  expect_error(f())
  
  expect_output(f(iType = 0L, vIndex = 0L, iIndex = 0L))
})

