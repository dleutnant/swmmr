test_that(".get_iIndex() works", {

  f <- swmmr:::.get_iIndex
  
  expect_error(f())

  expect_warning(expect_error(f(iType = 99L)))
  
  x <- list(subcatchments = data.frame(names = c("a", "b", "c")))
  
  result <- f(list_of_elements = x, iType = 0L, object_name = "b")
  
  expect_identical(result, list(iIndex = 1, names = "b"))
})
