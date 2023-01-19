test_that(".get_vIndex() works", {

  f <- swmmr:::.get_vIndex
  
  expect_error(f())

  expect_identical(
    f(iType = 0L, vIndex = 0L), 
    list(vIndex = 0, names = "rainfall_rate")
  )
})
