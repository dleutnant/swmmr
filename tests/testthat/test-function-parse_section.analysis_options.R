#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.analysis_options() works", {

  f <- swmmr:::parse_section.analysis_options
  
  expect_error(f())

  # TODO: check what should happen  
  
  x <- data.frame(
    option = paste0("opition_", 1:4),
    value = c("skip1", "skip2", "skip3", "option1")
  )

  f(x)
})
