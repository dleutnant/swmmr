test_that("assign_parameters.weirs() works", {

  f <- swmmr:::assign_parameters.weirs
  
  expect_error(f())
  
  x <- data.frame(
    Name = "a",
    FromNode = 1,
    ToNode = 2,
    Type = "type",
    CrestHt = 1,
    Cd = 1,
    Gated = 1,
    EC = 1,
    Cd2 = 1,
    Sur = 1
  )
  
  expect_identical(f(x), x)

})
