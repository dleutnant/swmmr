test_that("assign_parameters.subcatchments() works", {

  f <- swmmr:::assign_parameters.subcatchments
  
  expect_error(f())

  x <- data.frame(Name = "a", Outlet = 1, Area = 1)
  
  expect_silent(result <- f(x, subcatchment_typologies = NULL))
  expect_true("Rain_Gage" %in% names(result))
})
