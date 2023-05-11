#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("options_to_txt() works", {

  f <- swmmr:::options_to_txt
  
  expect_error(f())

  x <- list()
  class(x) <- "inp"
  
  expect_message(f(x), "section options is missing")
  
  x$options <- data.frame()
  
  path_out <- tempfile()
  stopifnot(dir.create(path_out))

  expect_message(f(x, "name", path_out), "txt file was written")
  expect_silent(f(x, "name", path_out, quiet = TRUE))
  
  file <- file.path(path_out, "txt", "name_options.txt")
  
  expect_file_exists(file)
  
  expect_identical(readLines(file), "[options]")
  
})
