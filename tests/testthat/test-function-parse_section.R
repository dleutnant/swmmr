#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section() works", {

  f <- swmmr:::parse_section
  
  expect_error(f())

  supported_classes <- gsub("parse_section.", "", grep(
    "^parse_section\\.", 
    ls(getNamespace("swmmr")), 
    value = TRUE
  ))

  # TODO: make this work  
  for (class in supported_classes) {
    x <- structure(list(), class = supported_classes[2L])
    expect_error(f(x))
  }

})
