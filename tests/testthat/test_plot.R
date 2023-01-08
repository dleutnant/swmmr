#source("./tests/testthat.R")
#source("./tests/testthat/helpers.R")

testthat::test_that("autoplot", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # Read all example input files
  inputs <- swmmr:::read_example_input_files()
  
  # autoplot contents of inp files
  suppressWarnings(list_of_ggplots <- purrr::map(
    inputs, 
    ggplot2::autoplot, 
    suppress_warnings = TRUE
  ))
  
  purrr::walk(list_of_ggplots, testthat::expect_s3_class, class = "gg")
})
