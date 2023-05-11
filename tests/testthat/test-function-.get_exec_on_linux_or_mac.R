test_that(".get_exec_on_linux_or_mac() works", {

  f <- swmmr:::.get_exec_on_linux_or_mac
  
  expect_message(f("no-such-path", "exe"))
  
  # Create three temporary file names
  files <- sapply(letters[1:3], tempfile, USE.NAMES = FALSE)
  
  # Create files for the second and third file name
  for (file in files[-1L]) {
    writeLines("test", file)
  }
  
  # The function should return the second path
  expect_identical(
    normalizePath(f(path = tempdir(), exe = basename(files))),
    normalizePath(files[2L])
  )
  
  unlink(files[-1L])
})
