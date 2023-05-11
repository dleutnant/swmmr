test_that(".onLoad() works", {

  suppressMessages(swmmr:::.onLoad())
  
  path <- getOption("swmmr.exec")
  
  expect_true(is.null(path) || file.exists(path))
  
})
