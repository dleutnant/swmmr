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
    "xsections"
  )
  
  expect_true(all(expected_names %in%  names(result)))
})
