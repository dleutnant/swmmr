test_that("weirs_to_sf() works", {

  f <- swmmr:::weirs_to_sf
  
  expect_error(f())

  x <- init_inp_list()
  
  expect_warning(f(x), "missing: weirs, coordinates")
  
  x$weirs = dirty_data_frame(
    Name = "ab",
    `From Node` = "a",
    `To Node` = "b"
  )
  
  x$coordinates = dirty_data_frame(
    Node = c("a", "b"),
    `X-Coord` = 1:2,
    `Y-Coord` = 2:3,
    pos = 1:2,
    id = 1:2
  )
  
  expect_inherits_sf(f(x))
})
