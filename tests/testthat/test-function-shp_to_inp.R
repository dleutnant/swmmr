test_that("shp_to_inp() works", {

  f <- swmmr:::shp_to_inp

  expect_error(suppressWarnings(f()))

})
