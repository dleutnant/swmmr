testthat::context("testing swmm io")

testthat::test_that("swmm_io", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # get the inp files
  inp_files <- system.file("extdata", paste0("Example", 1:6, ".inp"), 
                           package = "swmmr", mustWork = TRUE)
  
  # initially run the models and save results to temp file
  temp_file <- purrr::rerun(length(inp_files), tempfile())
  purrr::walk2(.x = inp_files, 
               .y = temp_file,
               ~ swmmr::run_swmm(.x, 
                                 rpt = paste0(.y, ".rpt"),
                                 out = paste0(.y, ".out")))
  
  # get the size of original rpt and out files
  orig_size_of_rpt <- file.size(paste0(temp_file, ".rpt"))
  orig_size_of_out <- file.size(paste0(temp_file, ".out"))
  
  # now read the models into R
  inp_obj <- purrr::map(inp_files, swmmr::read_inp)
  
  # create temp file names
  tmp_inp <- purrr::rerun(length(inp_obj), tempfile())
  
  # write the models back to file
  purrr::walk2(inp_obj, tmp_inp, ~ swmmr::write_inp(.x, .y))
  
  # run the new models
  purrr::walk(tmp_inp, swmmr::run_swmm)
  
  # get the size of new rpt and out files
  new_size_of_rpt <- file.size(paste0(tmp_inp, ".rpt"))
  new_size_of_out <- file.size(paste0(tmp_inp, ".out"))
  
  # remove files
  purrr::walk(c(tmp_inp, temp_file), ~ file.remove(list.files(tempdir(), 
                                                              full.names = TRUE,
                                                              pattern = basename(.))))
  
  # perform tests (with tolerance for rpt files due to tiny title change)
  testthat::expect_equal(orig_size_of_rpt, new_size_of_rpt, tolerance = 1e-3, info = "rpt check")
  testthat::expect_equal(orig_size_of_out, new_size_of_out, info = "out check")
  
})

testthat::test_that("rpt_reader", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # get the inp files
  inp_files <- system.file("extdata", paste0("Example", 1:6, ".inp"), 
                           package = "swmmr", mustWork = TRUE)
  
  # initially run the models and save results to temp file
  temp_file <- purrr::rerun(length(inp_files), tempfile())
  purrr::walk2(.x = inp_files, 
               .y = temp_file,
               ~ swmmr::run_swmm(.x, 
                                 rpt = paste0(.y, ".rpt"),
                                 out = paste0(.y, ".out")))

  # read rpt files
  list_of_rpt <- paste0(temp_file, ".rpt") %>% 
    purrr::map(swmmr::read_rpt)
  
  purrr::walk(list_of_rpt, testthat::expect_s3_class, class = "rpt")  
})

testthat::test_that("swmm error", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # get the inp files
  inp_file <- system.file("extdata", paste0("Example", 1, ".inp"), 
                           package = "swmmr", mustWork = TRUE)
  
  # read model
  inp <- swmmr::read_inp(inp_file)
  # set new parameters and update inp object
  # Error 1
  inp$timeseries <- transform(inp$timeseries, Name = "IDoNotExist")
  # Error 2
  inp$subcatchments <- transform(inp$subcatchments, 
                                 Name = sample(1:1e3, nrow(inp$subcatchments)))
  
  # write new inp file to disk
  tmp_inp <- tempfile()
  write_inp(inp, tmp_inp)
  
  # run swmm and expect error
  swmmr::run_swmm(inp = tmp_inp, 
                  rpt = paste0(tmp_inp, ".rpt"),
                  out = paste0(tmp_inp, ".out"))
  
  testthat::expect_message({
    rpt <- swmmr::read_rpt(paste0(tmp_inp, ".rpt"))
  })
  
  testthat::expect_s3_class(rpt, class = "rpt_error")
})

testthat::test_that("summary", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # get the inp files
  inp_files <- system.file("extdata", paste0("Example", 1:6, ".inp"), 
                           package = "swmmr", mustWork = TRUE)
  
  # now read the models into R
  inp_obj <- purrr::map(inp_files, swmmr::read_inp)
  
  purrr::walk(inp_obj, ~ testthat::expect_output(summary(.), 
                                            "summary of swmm model structure"))
  
})

