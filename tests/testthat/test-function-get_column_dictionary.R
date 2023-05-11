test_that("get_column_dictionary() works", {

  f <- swmmr:::get_column_dictionary
  
  result <- f()

  expect_true(is.data.frame(result))
  
  expected <- c("org_swmm", "shp_abb", "int_shp_to_inp")
  
  expect_true(all(expected %in% names(result)))
})
