test_that("CloseSwmmOutFile() works", {

  f <- swmmr:::CloseSwmmOutFile
  
  expect_identical(f(), 0L)

})

