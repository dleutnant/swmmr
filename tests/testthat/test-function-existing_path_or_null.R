test_that("existing_path_or_null() works", {

  f <- swmmr:::existing_path_or_null
  
  expect_null(f())
  expect_null(f("no-such-path"))
  expect_identical(f(tempdir()), tempdir())
})
