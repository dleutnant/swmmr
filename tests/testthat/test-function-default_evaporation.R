test_that("default_evaporation() works", {

  f <- swmmr:::default_evaporation
  
  expect_identical(f(), tibble::tibble(CONSTANT = 0, DRY_ONLY = 0))

})
