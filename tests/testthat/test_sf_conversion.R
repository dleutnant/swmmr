testthat::context("testing sf to inp conversion")

testthat::test_that("sf conversion", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # get the inp files
  inp_files <- system.file("extdata", paste0("Example", 1:6, ".inp"), 
                           package = "swmmr", mustWork = TRUE)
  
  # read models
  inp_obj <- purrr::map(inp_files, swmmr::read_inp)
  
  # get name and nelements per inp
  summaries_orig <- inp_obj %>% 
    purrr::map( ~ data.frame(section = names(.x), 
                             elements = purrr::map_int(.x, nrow), 
                             row.names = NULL, 
                             stringsAsFactors = FALSE)
    )
  
  
  # export models to temp dir
  temp_files <- purrr::rerun(length(inp_obj), tempfile())
  purrr::walk(temp_files, ~ dir.create(path = .))
  
  # inp to files
  purrr::pwalk(list(name = basename(inp_files), x = inp_obj, path_out = temp_files),
               function(name, x, path_out) swmmr::inp_to_files(x, 
                                                               name = name,
                                                               path_out = path_out))
  
  
  # helper function 
  path_or_null <- function(x) {
    if (length(x[1]) > 0 && file.exists(x[1])) x else NULL
  }

  # write all inputs into one list
  all_inputs <- list()
  all_inputs$path_options <- purrr::map2(temp_files, basename(inp_files), ~ path_or_null(file.path(.x, "txt", paste0(.y, "_options.txt"))))
  all_inputs$polygon_sf <-   purrr::map(inp_obj, ~ if("subcatchments" %in% names(.x)) {swmmr::subcatchments_to_sf(.x)} else NULL)
  all_inputs$point_sf <- purrr::map(inp_obj, ~ if("junctions" %in% names(.x)) {swmmr::junctions_to_sf(.x)} else NULL)
  all_inputs$outfall_sf <- purrr::map(inp_obj, ~ if("outfalls" %in% names(.x)) {swmmr::outfalls_to_sf(.x)} else NULL)
  all_inputs$line_sf <-  purrr::map(inp_obj, ~ if("conduits" %in% names(.x)) {swmmr::links_to_sf(.x)} else NULL)
  all_inputs$path_timeseries <- purrr::map2(temp_files, basename(inp_files), ~ path_or_null(list.files(file.path(.x,"dat"), "timeseries", full.names = TRUE)))
  all_inputs$pumps_sf <- purrr::map(inp_obj, ~ if("pumps" %in% names(.x)) {swmmr::pumps_to_sf(.x)} else NULL)
  all_inputs$path_pump_curve <- purrr::map2(temp_files, basename(inp_files), ~ path_or_null(file.path(.x,"txt", paste0(.y,"_PUMP_CURVE1.shp")))) # only valid for example 3
  all_inputs$weirs_sf <-  purrr::map(inp_obj, ~ if("weirs" %in% names(.x)) {swmmr::weirs_to_sf(.x)} else NULL)
  all_inputs$storage_sf <- purrr::map(inp_obj, ~ if("storages" %in% names(.x)) {swmmr::storages_to_sf(.x)} else NULL) 
  all_inputs$path_storage_curve <- purrr::map2(temp_files, basename(inp_files), ~ path_or_null(file.path(.x,"txt", paste0(.y,"_StorageCurve.txt"))))
  
  # sf_to_inp
  inp_converted <- purrr::pmap(all_inputs, ~ with( list(...), sf_to_inp(
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
    path_storage_curve = path_storage_curve)))

  
  # get name and nelements per inp
  summaries_converted <- inp_converted %>% 
    purrr::map( ~ data.frame(section = names(.x), 
                             elements = purrr::map_int(.x, nrow), 
                             row.names = NULL, 
                             stringsAsFactors = FALSE)
    )
  
  # compare the differences
  purrr::map2(summaries_orig, summaries_converted, ~ merge(.x,.y, by = "section"))
  
}
)