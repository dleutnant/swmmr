test_that("OpenSwmmOutFile() works", {

  f <- swmmr:::OpenSwmmOutFile
  
  expect_error(f())

  expect_output(result <- f("no-such-file"), "Could not open")
  expect_identical(result, list(error = 1L))
})
