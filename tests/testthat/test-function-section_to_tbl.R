#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("section_to_tbl() works", {
  
  f <- swmmr:::section_to_tbl
  
  expect_error(f())
  
  x <- c(
    "a\t1",
    "a\t2"
  )
  
  expect_s3_class(f(x, "options"), "data.frame")
  expect_warning(f(x, "unknown"))
  
})
