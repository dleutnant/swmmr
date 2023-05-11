test_that("create_temp_directories() works", {

  f <- swmmr:::create_temp_directories
  
  result <- f(n = 1L)
  
  expect_type(result, "character")
  expect_length(result, 1L)
  expect_true(dir.exists(result[[1L]]))
})
