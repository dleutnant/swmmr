test_that("default_report() works", {

  f <- swmmr:::default_report

  result <- f()
  
  expected <- c("INPUT", "CONTROLS", "SUBCATCHMENTS", "NODES", "LINKS")

  expect_true(all(expected %in% names(result)))
})
