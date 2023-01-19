test_that("input_to_list_of_sections() works", {

  f <- swmmr:::input_to_list_of_sections
  
  expect_error(f())
})
