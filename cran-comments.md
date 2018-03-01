## Package Update

This is a package update to swmmr 0.8.0.

## Test environments
* local OS X install, R 3.4.3
* r-hub x86_64-w64-mingw32 (64-bit), R 3.4.2
* r-hub x86_64-pc-linux-gnu (64-bit), R.3.4.2

## local R CMD check results

0 errors | 0 warnings | 0 note

## r-hub R CMD check results

0 errors | 1 warnings | 0 note

There is one warning because there is an error in re-building vignettes on CRAN.
The error occurs because the vignettes demonstrate how to use the package with 
the external software SWMM, which basically is an executable (i.e. 'swmm5.exe' 
on windows). However, SWMM is not installed on CRAN an therefore processing R 
code fails ("Could not find swmm executable.")

(see https://stat.ethz.ch/pipermail/r-package-devel/2018q1/002462.html)

## Reverse dependencies

There are no reverse dependencies.