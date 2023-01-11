#source("./tests/testthat.R")
#source("./tests/testthat/helpers.R")
#source("./R/test-helpers.R")

testthat::test_that("testing shp to inp conversion", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # read models from example input files
  inp_obj <- swmmr:::read_example_input_files()
  
  # provide the corresponding (base) file names
  inp_names <- names(inp_obj)
  
  # get name and nelements per inp
  summaries_orig <- inp_obj %>% 
    purrr::map( ~ data.frame(
      section = names(.x), 
      elements = purrr::map_int(.x, nrow), 
      row.names = NULL, 
      stringsAsFactors = FALSE
    ))
  
  # export models to temp dir
  #lapply(temp_files, unlink, recursive = TRUE) # Remove exsiting folders!
  temp_files <- create_temp_directories(n = length(inp_obj))
  
  # inp to files
  suppressWarnings(purrr::pwalk(
    list(x = inp_obj, name = inp_names, path_out = temp_files),
    function(x, name, path_out) {
      swmmr::inp_to_files(x, name, path_out, quiet = TRUE)
    }
  ))
  
  # helper function
  path_or_null <- function(...) swmmr:::existing_path_or_null(file.path(...))
  
  # shp_to_inp
  suppressWarnings(inp_converted <- purrr::map2(
    temp_files, 
    inp_names, ~ swmmr::shp_to_inp(
      path_options = path_or_null(.x, "txt", paste0(.y, "_options.txt")),
      path_polygon = path_or_null(.x, "shp", paste0(.y, "_polygon.shp")),
      path_point = path_or_null(.x,"shp", paste0(.y,"_point.shp")),
      path_outfall = path_or_null(.x,"shp", paste0(.y,"_outfall.shp")),
      path_line = path_or_null(.x,"shp", paste0(.y,"_link.shp")),
      path_timeseries = path_or_null(list.files(file.path(.x,"dat"), "timeseries", full.names = TRUE)),
      path_pumps = path_or_null(.x,"shp", paste0(.y,"_pumps.shp")),
      path_pump_curve = path_or_null(.x,"txt", paste0(.y,"_PUMP_CURVE1.shp")), # only valid for example 3
      path_weirs = path_or_null(.x,"shp", paste0(.y,"_weir.shp")),
      path_storage = path_or_null(.x,"shp", paste0(.y,"_storages.shp")),
      path_storage_curve = path_or_null(.x,"txt", paste0(.y,"_StorageCurve.txt"))) # only valid for example 6
  ))
  
  # get name and nelements per inp
  summaries_converted <- inp_converted %>% 
    purrr::map( ~ data.frame(
      section = names(.x), 
      elements = purrr::map_int(.x, nrow), 
      row.names = NULL, 
      stringsAsFactors = FALSE
    ))
  
  # compare the differences
  comparisons <- purrr::map2(summaries_orig, summaries_converted, ~ merge(
    .x,.y, by = "section"
  ))

  # in these sections there are differences!
  sections_with_diffs <- c(
    "infiltration",
    "polygons",
    "subareas",
    "subcatchments",
    "timeseries",
    "xsections"
  )
  
  expect_true(all(sapply(comparisons, function(x) {
    no_diff_expected <- !x$section %in% sections_with_diffs
    all(x[no_diff_expected, 2L] == x[no_diff_expected, 3L])
  })))
  
})
