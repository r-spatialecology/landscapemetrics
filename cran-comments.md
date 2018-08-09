## Resubmission to CRAN

* Thanks, please write package names, software names and API names in 
single quotes (e.g. 'FRAGSTATS') in your Description.
    * Done as suggested.

* Please add an URL for 'FRAGSTATS' in your Description text in the form
<http:...> or <https:...>
with angle brackets for auto-linking and no space after 'http:' and 
'https:'.
    * Done as suggested.

* We also see code lines such as: author Jeremy VanDerWal \email{jjvanderwal@@gmail.com} [...] 
author Florian Privé \email{florian.prive.21@gmail.com} [...] Please add all authors 
and copyright holders in the Authors@R field with.
the appropriate roles.
    * Done as suggested.

## New release

This package is a new release to CRAN.

## Test environments
* local OS X install, R 3.5.1
* ubuntu 18.04, R 3.5.1
* macOS High Sierra, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)
* Debian Linux, R-devel, GCC ASAN/UBSAN

## R CMD check results

0 errors | 0 warnings | 1 note

* It seems that on Linux architectures, the CHECK returns one NOTE because the libs subdirectory is then above the 1MB threshold. However, it seems that this NOTE only appears under Linux, but not under Windows or OSX. My understanding is that this inflation of the libs subdirectory is due to the use of Rcpp. Indeed, some functions of the landscapemetric package have been written in C++ using Rcpp. They are needed to perform a reasonable fast calculation of the landscape metrics. Without the speed up gained from those C++ functions, this package would become impractical.

## Reverse dependencies

There are currently no reverse dependencies.
