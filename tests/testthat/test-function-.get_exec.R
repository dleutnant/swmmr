test_that(".get_exec() works", {

  suppressMessages(result <- swmmr:::.get_exec())
  
  expect_true(is.null(result) || file.exists(result))

})
