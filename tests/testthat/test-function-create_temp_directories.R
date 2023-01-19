test_that("create_temp_directories() works", {

  f <- swmmr:::create_temp_directories
  
  expect_error(f())

  result <- f(1L)
  
  expect_type(result, "list")
  expect_length(result, 1L)
  expect_true(dir.exists(result[[1L]]))
})
