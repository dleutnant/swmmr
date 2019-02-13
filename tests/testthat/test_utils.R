context("test_utils")

test_that("clean_stop() works", {
  expect_error(clean_stop())
})

test_that("clean_warning() works", {
  expect_warning(clean_warning())
})

test_that("stop_on_bad_index() works", {
  expect_silent(stop_on_bad_index(1L, LETTERS))
  expect_error(stop_on_bad_index(1.3, LETTERS))
  expect_error(stop_on_bad_index(-1, LETTERS))
  expect_error(stop_on_bad_index(26, LETTERS))
})
