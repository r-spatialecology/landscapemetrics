## New release

This package is a new release to CRAN.

## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* It seems that on Linux architectures, the CHECK returns one NOTE because the libs subdirectory is then above the 1MB threshold. However, it seems that this NOTE only appears under Linux, but not under Windows or OSX. My understanding is that this inflation of the libs subdirectory is due to the use of Rcpp. Indeed, some functions of the landscapemetric package have been written in C++ using Rcpp. They are needed to perform a reasonable fast calculation of the landscape metrics. Without the speed up gained from those C++ functions, this package would become impractical.

## Reverse dependencies

There are currently no reverse dependencies.
