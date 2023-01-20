test_that("shp_to_inp() works", {

  f <- swmmr:::shp_to_inp

  suppressWarnings(expect_warning(result <- f()))

  expect_inherits(result, "inp")  

  expected <- c("options", "evaporation", "timeseries", "report")
  
  expect_true(all(expected %in% names(result)))
})
