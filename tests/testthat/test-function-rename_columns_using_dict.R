test_that("rename_columns_using_dict() works", {

  f <- swmmr:::rename_columns_using_dict
  
  expect_error(f())

  df <- data.frame(a = 1:2)
  
  expect_identical(f(df, from = "shp_abb"), df)
  
  df$RainGag <- c("a", "b")
  
  result <- f(df, from = "shp_abb")
  
  expect_identical(df[["RainGag"]], result[["Rain_Gage"]])
})
