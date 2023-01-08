#source("./tests/testthat.R")
#source("./tests/testthat/helpers.R")
#source("./R/test-helpers.R")

testthat::context("testing sf to inp conversion")

testthat::test_that("sf conversion", {
  
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
  #lapply(temp_files, unlink, recursive = TRUE)
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
  
  # write all inputs into one list
  all_inputs <- list(
    path_options = purrr::map2(temp_files, inp_names, ~ path_or_null(.x, "txt", paste0(.y, "_options.txt"))),
    polygon_sf = purrr::map(inp_obj, ~ if("subcatchments" %in% names(.x)) swmmr::subcatchments_to_sf(.x)),
    point_sf = purrr::map(inp_obj, ~ if("junctions" %in% names(.x)) swmmr::junctions_to_sf(.x)),
    outfall_sf = purrr::map(inp_obj, ~ if("outfalls" %in% names(.x)) swmmr::outfalls_to_sf(.x)),
    line_sf = purrr::map(inp_obj, ~ if("conduits" %in% names(.x)) swmmr::links_to_sf(.x)),
    path_timeseries = purrr::map2(temp_files, inp_names, ~ path_or_null(list.files(file.path(.x,"dat"), "timeseries", full.names = TRUE))),
    pumps_sf = purrr::map(inp_obj, ~ if("pumps" %in% names(.x)) swmmr::pumps_to_sf(.x)),
    path_pump_curve = purrr::map2(temp_files, inp_names, ~ path_or_null(.x,"txt", paste0(.y,"_PUMP_CURVE1.shp"))), # only valid for example 3
    weirs_sf = purrr::map(inp_obj, ~ if("weirs" %in% names(.x)) swmmr::weirs_to_sf(.x)),
    storage_sf = purrr::map(inp_obj, ~ if("storages" %in% names(.x)) swmmr::storages_to_sf(.x)),
    path_storage_curve = purrr::map2(temp_files, inp_names, ~ path_or_null(.x,"txt", paste0(.y,"_StorageCurve.txt")))
  )
  
  # sf_to_inp
  inp_converted <- purrr::pmap(all_inputs, ~ with( 
    list(...), 
    suppressWarnings(sf_to_inp(
      path_options = path_options,
      polygon_sf = polygon_sf,
      point_sf = point_sf,
      outfall_sf = outfall_sf,
      line_sf = line_sf,
      path_timeseries = path_timeseries,
      pumps_sf = pumps_sf,
      path_pump_curve = path_pump_curve,
      weirs_sf = weirs_sf,
      storage_sf = storage_sf,
      path_storage_curve = path_storage_curve
    ))
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
    "coordinates", 
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
