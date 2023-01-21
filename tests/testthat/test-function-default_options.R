test_that("default_options() works", {

  f <- swmmr:::default_options

  result <- f()

  # all names are expected to be in upper case (each letter)
  expect_identical(names(result), toupper(names(result)))

})
