testthat::context("testing swmm io")

testthat::test_that("swmm_io", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # get the inp files
  inp_files <- grep("inp", list.files("~/EPA_SWMM_Projects/Examples",
                                      full.names = TRUE),
                    value = TRUE)
  
  # initially run the models and save results to temp file
  temp_file <- purrr::rerun(length(inp_files), tempfile())
  purrr::walk2(.x = inp_files, 
               .y = temp_file,
               ~ swmmr::run_swmm(.x, 
                                 rpt = paste0(.y, ".rpt"),
                                 out = paste0(.y, ".out")))
  
  # get the size of original rpt and out files
  orig_size_of_rpt <- file.size(purrr::map_chr(temp_file, ~ paste0(., ".rpt")))
  orig_size_of_out <- file.size(purrr::map_chr(temp_file, ~ paste0(., ".out")))
  
  # now read the models into R
  inp_obj <- purrr::map(inp_files, swmmr::read_inp)
  
  # create temp file names
  tmp_inp <- purrr::rerun(length(inp_obj), tempfile())
  
  # write the models back to file
  purrr::walk2(inp_obj, tmp_inp, ~ swmmr::write_inp(.x, .y))
  
  # run the new models
  purrr::walk(tmp_inp, swmmr::run_swmm)
  
  # get the size of new rpt and out files
  new_size_of_rpt <- file.size(purrr::map_chr(tmp_inp, ~ paste0(., ".rpt")))
  new_size_of_out <- file.size(purrr::map_chr(tmp_inp, ~ paste0(., ".out")))
  
  # remove files
  purrr::walk(c(tmp_inp, temp_file), ~ file.remove(list.files(tempdir(), 
                                                              full.names = TRUE,
                                                              pattern = basename(.))))
  
  # perform tests (with tolerance for rpt files due to tiny title change)
  testthat::expect_equal(orig_size_of_rpt, new_size_of_rpt, tolerance = 1e-3, info = "rpt check")
  testthat::expect_equal(orig_size_of_out, new_size_of_out, info = "out check")
  
})