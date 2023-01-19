test_that("summary.inp() works", {

  f <- swmmr:::summary.inp
  
  expect_error(f())

  x <- list(options = data.frame(
    Option = c(
      "infiltration", 
      "flow_units",
      "flow_routing", 
      "start_date", 
      "end_date"
    ),
    Value = c(
      "true",
      "m3/s",
      "fr",
      "sd",
      "ed"
    )
  ))
  
  expect_output(f(x))
})
