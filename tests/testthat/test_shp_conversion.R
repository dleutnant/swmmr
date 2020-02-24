testthat::context("testing swmm conversion")

testthat::test_that("shp conversion", {
  
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
  
  # shp_to_inp
  inp_converted <- purrr::map2(temp_files, basename(inp_files), ~ 
                                 swmmr::shp_to_inp(path_options = path_or_null(file.path(.x, "txt", paste0(.y, "_options.txt"))),
                                                   path_polygon = path_or_null(file.path(.x, "shp", paste0(.y, "_polygon.shp"))),
                                                   path_point = path_or_null(file.path(.x,"shp", paste0(.y,"_point.shp"))),
                                                   path_outfall = path_or_null(file.path(.x,"shp", paste0(.y,"_outfall.shp"))),
                                                   path_line = path_or_null(file.path(.x,"shp", paste0(.y,"_link.shp"))),
                                                   path_timeseries = path_or_null(list.files(file.path(.x,"dat"), "timeseries", full.names = TRUE)),
                                                   path_pumps = path_or_null(file.path(.x,"shp", paste0(.y,"_pumps.shp"))),
                                                   path_pump_curve = path_or_null(file.path(.x,"txt", paste0(.y,"_PUMP_CURVE1.shp"))), # only valid for example 3
                                                   path_weirs = path_or_null(file.path(.x,"shp", paste0(.y,"_weir.shp"))),
                                                   path_storage = path_or_null(file.path(.x,"shp", paste0(.y,"_storages.shp"))),
                                                   path_storage_curve = path_or_null(file.path(.x,"txt", paste0(.y,"_StorageCurve.txt"))))) # only valid for example 6
  
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