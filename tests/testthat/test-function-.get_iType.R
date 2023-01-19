test_that(".get_iType() works", {

  f <- swmmr:::.get_iType
  
  expect_identical(f(0L), list(iType = 0L, names = "subcatchment"))

})
