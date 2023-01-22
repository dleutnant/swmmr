test_that("unload_dll() works", {

  skip_on_ci()
  
  expect_silent(swmmr:::unload_dll())

})

