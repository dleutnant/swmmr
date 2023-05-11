test_that("read_example_input_files() works", {

  f <- swmmr:::read_example_input_files
  
  result <- f()
  
  expect_type(result, "list")
  expect_true(all(grepl("^Example.*\\.inp$", names(result))))
  expect_true(all(sapply(result, inherits, "inp")))
  
})
