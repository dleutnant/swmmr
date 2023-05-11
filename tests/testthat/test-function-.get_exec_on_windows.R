test_that(".get_exec_on_windows() works", {

  f <- swmmr:::.get_exec_on_windows
  
  expect_error(f())
  
  expect_message(f("no-such-path", "prefix", "exe"))
})
