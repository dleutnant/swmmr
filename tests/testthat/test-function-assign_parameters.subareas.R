test_that("assign_parameters.subareas() works", {

  f <- swmmr:::assign_parameters.subareas
  
  expect_error(f())

  x <- data.frame(
    Name = "a",
    N_Imperv = 1,
    N_Perv = 1,
    S_Imperv = 1
  )
  
  expect_silent(result <- f(x, subcatchment_typologies = NULL))

  expect_true("Subcatchment" %in% names(result))
  
})
