test_that("inp_to_sf() works", {

  f <- swmmr:::inp_to_sf
  
  expect_error(f())

  x <- swmmr:::read_example_input_files(ids = 1L)[[1L]]
  
  suppressWarnings(result <- f(x))
  
  expected_elements <- c(
    "subcatchments", "junctions", "outfalls", "links", "raingages"
  )
  
  expect_true(all(expected_elements %in% names(result)))
})
