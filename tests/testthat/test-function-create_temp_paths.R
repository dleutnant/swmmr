test_that("create_temp_paths() works", {

  f <- swmmr:::create_temp_paths
  
  expect_error(f())

  result <- f(1L)
  
  expect_false(file.exists(result[[1L]]))
  expect_true(dir.exists(dirname(result[[1L]])))
  
})
