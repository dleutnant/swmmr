test_that("system_file() works", {

  f <- swmmr:::system_file
  
  expect_identical(f(), system.file(package = "swmmr"))

})
