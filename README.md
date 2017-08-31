
<!-- README.md is generated from README.Rmd. Please edit that file -->
swmmr
=====

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/swmmr)](https://cran.r-project.org/package=swmmr) [![Build Status](https://travis-ci.org/dleutnant/swmmr.svg?branch=dev)](https://travis-ci.org/dleutnant/swmmr)

Functions to connect the widely used [Storm Water Management Model (SWMM)](https://www.epa.gov/water-research/storm-water-management-model-swmm) of the United States Environmental Protection Agency (US EPA) to R with currently two main goals: (1) Run a SWMM simulation from R and (2) provide fast access to simulation results, i.e. SWMM's binary '.out'-files. High performance is achieved with help of Rcpp. Additionally, reading SWMM's '.inp'-files is supported to glance model structures.

Installation
------------

Installation is easy thanks to CRAN:

``` r
#install.packages("swmmr")
```

You can install the dev version from github with:

``` r
# install.packages("devtools")
#devtools::install_github("dleutnant/swmmr@dev")
```

Example
-------

This is a basic example which shows you how to work with the package. We use the example shipped with the SWMM5 executable.

``` r

library(swmmr)
library(purrr) # to conveniently work with list objects

# set path to inp (avoid white spaces in file paths!)
inp_path <- "~/EPA_SWMM_Projects/Examples/Example1.inp"

# glance model structure, the result is a list of data.frames with SWMM sections
inp <- read_inp(inp = inp_path)

# available SWMM sections
summary(inp)
#>               Length Class      Mode
#> TITLE         2      data.frame list
#> OPTIONS       2      data.frame list
#> RAINGAGES     6      data.frame list
#> SUBCATCHMENTS 8      data.frame list
#> SUBAREAS      7      data.frame list
#> INFILTRATION  5      data.frame list
#> JUNCTIONS     6      data.frame list
#> OUTFALLS      4      data.frame list
#> CONDUITS      8      data.frame list
#> XSECTIONS     7      data.frame list
#> POLLUTANTS    9      data.frame list
#> LANDUSES      1      data.frame list
#> COVERAGES     3      data.frame list
#> LOADINGS      6      data.frame list
#> BUILDUP       7      data.frame list
#> WASHOFF       7      data.frame list
#> TIMESERIES    3      data.frame list
#> REPORT        2      data.frame list
#> TAGS          6      data.frame list
#> COORDINATES   3      data.frame list
#> VERTICES      3      data.frame list
#> Polygons      3      data.frame list
#> SYMBOLS       3      data.frame list
#> BACKDROP      5      data.frame list

# for example, inspect section TIMESERIES
inp$TIMESERIES
#>    Name  Date Time
#> 1   TS1  0:00 0.00
#> 2   TS1  1:00 0.25
#> 3   TS1  2:00 0.50
#> 4   TS1  3:00 0.80
#> 5   TS1  4:00 0.40
#> 6   TS1  5:00 0.10
#> 7   TS1  6:00 0.00
#> 8   TS1 27:00 0.00
#> 9   TS1 28:00 0.40
#> 10  TS1 29:00 0.20
#> 11  TS1 30:00 0.00

# run a simulation
# the result is a named list of paths, directing
# to the inp, rpt and out-file, respectively.
files <- run_swmm(inp = inp_path)
#> arguments 'minimized' and 'invisible' are for Windows only

... EPA-SWMM 5.1 (Build 5.1.012)

 o  Retrieving project data
 o  Simulation complete           

... EPA-SWMM completed in 0.00 seconds.

# we can now read model results from the binary output:
# here, we focus on the system variable (iType = 3) from which we pull
# total rainfall (in/hr or mm/hr) and total runoff (flow units) (vIndex = c(1,4)).
results <- read_out(files$out, iType = 3, vIndex = c(1, 4))

# results is a list object containing two time series 
str(results, max.level = 2)
#> List of 1
#>  $ system_variable:List of 2
#>   ..$ total_rainfall:An 'xts' object on 1998-01-01 01:00:00/1998-01-02 12:00:00 containing:
#>   Data: num [1:36, 1] 0.25 0.5 0.8 0.4 0.1 ...
#>   Indexed by objects of class: [POSIXct,POSIXt] TZ: GMT
#>   xts Attributes:  
#>  NULL
#>   ..$ total_runoff  :An 'xts' object on 1998-01-01 01:00:00/1998-01-02 12:00:00 containing:
#>   Data: num [1:36, 1] 0 6.22 13.03 24.25 14.17 ...
#>   Indexed by objects of class: [POSIXct,POSIXt] TZ: GMT
#>   xts Attributes:  
#>  NULL

# basic summary
results[[1]] %>% purrr::map(summary)
#> $total_rainfall
#>      Index                        .x[[i]]       
#>  Min.   :1998-01-01 01:00:00   Min.   :0.00000  
#>  1st Qu.:1998-01-01 09:45:00   1st Qu.:0.00000  
#>  Median :1998-01-01 18:30:00   Median :0.00000  
#>  Mean   :1998-01-01 18:30:00   Mean   :0.07361  
#>  3rd Qu.:1998-01-02 03:15:00   3rd Qu.:0.00000  
#>  Max.   :1998-01-02 12:00:00   Max.   :0.80000  
#> 
#> $total_runoff
#>      Index                        .x[[i]]       
#>  Min.   :1998-01-01 01:00:00   Min.   : 0.0000  
#>  1st Qu.:1998-01-01 09:45:00   1st Qu.: 0.0000  
#>  Median :1998-01-01 18:30:00   Median : 0.0000  
#>  Mean   :1998-01-01 18:30:00   Mean   : 2.1592  
#>  3rd Qu.:1998-01-02 03:15:00   3rd Qu.: 0.1033  
#>  Max.   :1998-01-02 12:00:00   Max.   :24.2530

# basic plotting
results[[1]] %>% purrr::imap( ~ plot(.x, main = .y))
#> $total_rainfall
```

![](README-example-1.png)

    #> 
    #> $total_runoff

![](README-example-2.png)

Acknowledgments
---------------

This package has been developed in the course of the project [STBMOD](https://www.fh-muenster.de/forschung/forschungskatalog/projekt.php?pr_id=722), carried out at the [Institute for Infrastructure, Water, Resources, Environment (IWARU)](https://en.fh-muenster.de/iwaru/index.php) of the [Muenster University of Applied Sciences](https://www.fh-muenster.de). The project was funded by the German Federal Ministry of Education and Research (BMBF, FKZ 03FH033PX2).

The development of the R package was inspired by the work of [Peter Steinberg](https://github.com/PeterDSteinberg/RSWMM). Also, it benefits from SWMM Interface Guide.
