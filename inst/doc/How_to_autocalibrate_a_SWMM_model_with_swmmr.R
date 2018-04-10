## ----setup, include = FALSE----------------------------------------------
calibration_res <- NULL
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  eval = !identical(Sys.getenv("NO_VIGNETTE_ON_CRAN"), "") # set to FALSE to prevent eval on CRAN!
)

## ----load_libs-----------------------------------------------------------
#  library(swmmr)
#  library(DEoptim)

## ----model_setup---------------------------------------------------------
#  # set path to inp
#  # If your operating system is Windows, the Example1.inp model is usually
#  # located at "C:\Users\your user name\Documents\EPA SWMM Projects\Examples".
#  # For convenience the Example1.inp model is also included in the swmmr package.
#  # Feel free to change this to your path of choice.
#  inp_file <- system.file("extdata", "Example1.inp", package = "swmmr", mustWork = TRUE)
#  
#  # both rpt and out files are temporary files
#  tmp_rpt_file <- tempfile()
#  tmp_out_file <- tempfile()
#  
#  # initiate the simulation
#  swmm_files <- run_swmm(
#    inp = inp_file,
#    rpt = tmp_rpt_file,
#    out = tmp_out_file
#  )

## ----obs-----------------------------------------------------------------
#  obs <- read_out(
#    file = swmm_files$out,
#    iType = 1,
#    object_name = "18",
#    vIndex = 4
#  )[["18"]]$total_inflow

## ----sim_and_read--------------------------------------------------------
#  # read model structure
#  inp <- read_inp(swmm_files$inp)
#  
#  # show the original parameter values
#  inp$subcatchments[inp$subcatchments$Area > 10, ]
#  

## ----gof-----------------------------------------------------------------
#  # function calculates the goodness of fit value
#  # input x is a two column xts object, col1: obs, col2: sim
#  nse <- function(x) {
#    1 - sum((x[, 1] - x[, 2]) ^ 2) / sum((x[, 1] - mean(x[, 1])) ^ 2)
#  }

## ----obj_fun-------------------------------------------------------------
#  obj_fun <- function(x, inp, obs) {
#  
#    # set new parameters and update inp object
#    inp$subcatchments <- transform(
#      inp$subcatchments,
#      Perc_Imperv = ifelse(Area > 10, x, Perc_Imperv)
#    )
#  
#    # write new inp file to disk
#    tmp_inp <- tempfile()
#    write_inp(inp, tmp_inp)
#  
#    # run swmm with new parameter set
#    swmm_files <- suppressMessages(run_swmm(tmp_inp, stdout = NULL))
#  
#    # remove files when function exits to avoid heavy disk usage
#    on.exit(file.remove(unlist(swmm_files)))
#  
#    # read sim result
#    sim <- read_out(
#      file = swmm_files$out, # path to out file
#      iType = 1, # type: node
#      object_name = "18", # name of node
#      vIndex = 4 # parameter at node: total inflow
#    )[["18"]]$total_inflow # directly access to xts object
#  
#    # calculate goodness-of-fit
#    # note: multiply by minus one to have a real min problem (nse: +1 to -Inf)
#    nse(merge(obs, sim)) * -1
#  }
#  

## ----optim---------------------------------------------------------------
#    set.seed(84) # to get reproducible results
#  
#    calibration_res <- DEoptim(
#      fn = obj_fun,
#      lower = c(0, 0),
#      upper = c(100, 100),
#      control = list(
#        itermax = 50, # maximum iterations
#        trace = 10, # print progress every 10th iteration
#        packages = c("swmmr"), # export packages to optimization environment
#        parVar = c("nse"), # export function to optimization environment
#        parallelType = 0 # set to 1 to use all available cores
#      ),
#      inp = inp, # 'inp' object
#      obs = obs # xts object containing observation data
#    )
#  
#    summary(calibration_res)

