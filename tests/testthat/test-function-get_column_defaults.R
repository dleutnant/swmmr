#library(testthat)

test_that("get_column_defaults() works", {

  result <- swmmr:::get_column_defaults()
  
  expect_type(result, "list")
  
  expected_names <- c(
    "subcatchments", 
    "subareas", 
    "infiltration_horton",
    "infiltration_green_ampt", 
    "junction",
    "outfalls", 
    "conduits",
    "xsections",
    "options",
    "report",
    "evaporation"
  )
  
  expect_true(all(expected_names %in%  names(result)))
  
  # (formerly: swmmr:::default_evaporation)
  expect_identical(
    tibble::as_tibble(result$evaporation), 
    tibble::tibble(CONSTANT = 0, DRY_ONLY = 0)
  )

  # (formerly: swmmr:::default_options)
  # all names are expected to be in upper case (each letter)
  expect_identical(names(result$options), toupper(names(result$options)))

  # (formerly: swmmr:::default_report)
  expected <- c("INPUT", "CONTROLS", "SUBCATCHMENTS", "NODES", "LINKS")
  expect_true(all(expected %in% names(result$report)))
  
})
