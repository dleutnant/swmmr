testthat::test_that("autoplot", {
  
  # only local tests
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  # get the inp files
  inp_files <- system.file("extdata", paste0("Example", 1:6, ".inp"), 
                           package = "swmmr", mustWork = TRUE)
  
  # read and autoplot inp files
  list_of_ggplots <- inp_files %>% 
    purrr::map(swmmr::read_inp) %>% 
    purrr::map(ggplot2::autoplot)
  
  purrr::walk(list_of_ggplots, testthat::expect_s3_class, class = "gg")
  
})