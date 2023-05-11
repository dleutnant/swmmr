#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.conduit_surcharge_summary() works", {

  f <- swmmr:::parse_section.conduit_surcharge_summary
  
  expect_error(f())

  indices <- 1:7
  x <- data.frame(name = letters[indices], value = indices)
  
  result <- f(x)
  
  expect_names(result, c(
    "Conduit", "Hours_Full_Both_Ends", "Hours_Full_Upstream",
    "Hours_Full_Dnstream", "Hours_Above_Full_Normal_Flow", 
    "Hours_Capacity_Limited"
  ))

  expect_nrow(result, 2L)
})
