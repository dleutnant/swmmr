test_that(".get_os() works", {

  f <- swmmr:::.get_os
  
  expect_type(f(), "character")

})
