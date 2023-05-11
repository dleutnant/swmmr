dirty_data_frame <- function(...) {
  data.frame(..., check.names = FALSE)
}

init_inp_list <- function(...) {
  structure(list(...), class = "inp")
}

expect_data_frame <- function(x, nrow = NULL, names = NULL) {
  expect_inherits(x, "data.frame")
  if (!is.null(nrow)) {
    expect_nrow(x, nrow)
  }
  if (!is.null(names)) {
    expect_names(x, names)
  }
}

expect_file_exists <- function(file = file.path(...), ...) {
  expect_true(file.exists(file))
}

expect_inherits <- function(x, what) {
  expect_true(inherits(x, what))
}

expect_inherits_sf <- function(x) {
  expect_inherits(x, "sf")
}

expect_names <- function(x, nm) {
  expect_true(all(nm %in% names(x)))
}

expect_nrow <- function(x, n) {
  expect_identical(nrow(x), n)
}
