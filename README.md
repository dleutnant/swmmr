# swmmr
R library for US EPA's SWMM (>=5.1.010)

Currently, the package includes functions to read SWMM's binary output files and
to inspect SWMM's input files. High performance is achieved with help of Rcpp.


This package has been developed in the course of the project "STBMOD" 
(https://www.fh-muenster.de/forschung/forschungskatalog/projekt.php?pr_id=722), 
funded by the German Federal Ministry of Education and Research (BMBF, FKZ 03FH033PX2).

Install using devtools:

``` r
if(!require(devtools)) {
  install.packages('devtools')
  devtools::install_github("dleutnant/swmmr")
}
```