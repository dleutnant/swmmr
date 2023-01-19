test_that("example_input_files() works", {

  f <- swmmr:::example_input_files
  
  result <- f()

  expect_type(result, "character")

  expect_true(all(file.exists(result)))
})
