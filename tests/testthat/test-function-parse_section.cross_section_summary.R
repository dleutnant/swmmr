#source("tests/testthat.R")
#source("tests/testthat/helpers_general.R")

test_that("parse_section.cross_section_summary() works", {

  f <- swmmr:::parse_section.cross_section_summary
  
  expect_error(f())

  indices <- 1:7
  x <- data.frame(name = paste0("name_", indices), value = indices)
  
  result <- f(x)

  expect_data_frame(result)
  expect_nrow(result, 2L)
  expect_names(result, c(
    "Conduit", 
    "Shape", 
    "Full_Depth", 
    "Full_Area", 
    "Hyd_Rad",  
    "Max_Width", 
    "No_of_Barrels", 
    "Full_Flow"
  ))
  
})
