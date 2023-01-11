test_that("check_pkg_avail() works", {

  f <- swmmr:::check_pkg_avail
  
  expect_error(f())

  expect_error(f("no-such-package"))
  expect_silent(f("swmmr"))
})
