test_that("get_temp_paths() works", {

  f <- swmmr:::get_temp_paths
  
  result <- f(n = 1L)
  
  expect_false(file.exists(result[1L]))
  expect_true(dir.exists(dirname(result[1L])))
  
})
