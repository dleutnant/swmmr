test_that("unload_dll() works", {

  skip_on_ci()
  
  expect_error(swmmr:::unload_dll(), "was not loaded")
  
})
