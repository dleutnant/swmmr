test_that("stop_on_bad_index() works", {

  f <- swmmr:::stop_on_bad_index
  
  expect_error(f())

  choices <- letters[1:3]
  
  expect_error(f("a", choices))
  expect_error(f(3, choices))
  expect_error(f(1:2, choices))
  
  expect_silent(f(0, choices))
  
})
