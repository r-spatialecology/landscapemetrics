# landscapemetrics 2.1.0
Larger internal updates to several algorithms

# landscapemetrics 2.0.0 - resubmission
This is a re-submission. The below error has now been fixed. 

Possibly misspelled words in DESCRIPTION: functionn (44:53)

# landscapemetrics 2.0.0
Major update removing dependency to raster and sp package

# landscapemetrics 1.5.6
This is a re-submission. The below error has now been fixed
`Specified C++11: please drop specification unless essential`
Remove dependencies

# landscapemetrics 1.5.5
- Improved user experience
- Fix typo in Maintainer name

# landscapemetrics 1.5.4
Computational improvements

# landscapemetrics 1.5.3
Minor bug fixes and improved terra support

# landscapemetrics 1.5.2
Minor bug fixes

# landscapemetrics 1.5.1
Small improvements package CI and bug fixes

# landscapemetrics 1.5.0
Better handling of input data, new functions and terra support

# landscapemetrics 1.4.7
Adding new function

# landscapemetrics 1.4.6
Minor improvements of existing functions

# landscapemetrics 1.4.5
Minor improvements, bug fixes and new functions

# landscapemetrics 1.4.4
Minor improvements and bug fixes

# landscapemetrics 1.4.3
Improvements of existing functions

# landscapemetrics 1.4.2
Minor improvements and bug fixes

# landscapemetrics 1.4.1
Major bugfixes

# landscapemetrics 1.4
Improvements of existing functions and make sure all CRAN checks run

# landscapemetrics 1.3.2
Make sure all CRAN checks run

## Update Version: 1.3.1
Minor bugfixes

## Update Version: 1.3
Minor bugfixes

## Update Version: 1.2.2
Small bugfixes and improvements of existing functions

## Update Version: 1.2.1
Small bugfixes

## Update Version: 1.2
Minor bugfixes and improvments of existing functions

## Update Version: 1.1
Minor bugfixes and improvments of existing functions

## Update Version: 1.0
Minor bugfixes, less dependencies and new function

## Update Version: 0.3.1
Minor bugfixes

## Update Version: 0.3
Minor bugfixes, better memory allocation and new functions

## Update Version: 0.2
Minor bugfixes and new functions

## Update Version: 0.1.1
a) The Solaris installation error is described in §1.6.4 of 'Writing R 
Extensions': please study it.

* Done as suggested

b) You use isFALSE from R 3.5.x despite claiming Depends: R (≥ 3.1): you 
cannot have actually tested that.

* Done as suggested

c) There are problems with the tests showing in the 'Additional issues' (shortly if not already).  
The way you write your tests, the reports such as

── 1. Failure: lsm_c_lsi results are equal to fragstats (@test-lsm-c-lsi.R#7)  ─
 all(...) isn't true.

 ── 2. Failure: lsm_c_split results are equal to fragstats (@test-lsm-c-split.R#7
 all(...) isn't true.

 ── 3. Failure: lsm_p_enn results are comparable to fragstats (@test-lsm-p-enn.R#
 all(...) isn't true.

are completely useless. But testing numerical results by rounding rather than using all.equal 
is bad practice (and it is not you who is paying the price!).

You could use something like

> all.equal(sort(fragstats_class_landscape_lsi), sort(landscapemetrics_class_landscape_lsi$value))
[1] "Mean relative difference: 4.816766e-06"

with a suitable tolerance.

* The software we compare our results to (FRAGSTATS) has only a precision of 4 digits itself. We discussed using relative differences but decided that we do not want to show that our results are equal to the FRAGSTATS results within a tolerance, but rather that our results would be exactly the same assuming the same precision. Therefore we decided to round our results for the tests.

## Update Version: 0.1.0
First submission to CRAN
