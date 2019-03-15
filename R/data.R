#' Example map (random cluster neutral landscape model).
#'
#' An example map to show landscapetools functionality
#' generated with the `nlm_randomcluster()` algorithm.
#'
#' @format A raster layer object.
#' @source Simulated neutral landscape model with R. https://github.com/ropensci/NLMR/
"landscape"

#' Augusta NLCD 2011
#'
#' A real landscape of area near Augusta, Georgia obtained from the National
#' Land Cover Database (NLCD)
#'
#' @format A raster layer object.
#' @source https://www.mrlc.gov/nlcd2011.php
#' @references Homer, C.G., Dewitz, J.A., Yang, L., Jin, S., Danielson, P., Xian, G., Coulston, J., Herold, N.D., Wickham, J.D., and Megown, K., 2015, Completion of the 2011 National Land Cover Database for the conterminous United States-Representing a decade of land cover change information. Photogrammetric Engineering and Remote Sensing, v. 81, no. 5, p. 345-354
"augusta_nlcd"

#' Podlasie ESA CCI LC
#'
#' A real landscape of the Podlasie region in Poland from the ESA CCI Land
#' Cover
#'
#' @format A raster layer object.
#' @source http://maps.elie.ucl.ac.be/CCI/viewer/
"podlasie_ccilc"

#' Fragstats results for landscapemetrics::landscape (patch level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on patch
#' level.
#'
#' @format A tibble object.
"fragstats_patch_landscape"

#' Fragstats results for landscapemetrics::augusta_nlcd (patch level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on patch
#' level.
#'
#' @format A tibble object.
"fragstats_patch_augusta_nlcd"

#' Fragstats results for landscapemetrics::podlasie (patch level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on patch
#' level.
#'
#' @format A tibble object.
"fragstats_patch_podlasie"

#' Fragstats results for landscapemetrics::landscape (class level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on class
#' level.
#'
#' @format A tibble object.
"fragstats_class_landscape"

#' Fragstats results for landscapemetrics::augusta_nlcd (class level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on class
#' level.
#'
#' @format A tibble object.
"fragstats_class_augusta_nlcd"

#' Fragstats results for landscapemetrics::podlasie (class level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on class
#' level.
#'
#' @format A tibble object.
"fragstats_class_podlasie"


#' Fragstats results for landscapemetrics::landscape (landscape level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on landscape
#' level.
#'
#' @format A tibble object.
"fragstats_landscape_landscape"

#' Fragstats results for landscapemetrics::augusta_nlcd (landscape level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on landscape
#' level.
#'
#' @format A tibble object.
"fragstats_landscape_augusta_nlcd"

#' Fragstats results for landscapemetrics::podlasie_ccilc (landscape level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on landscape
#' level.
#'
#' @format A tibble object.
"fragstats_landscape_podlasie"

#' Tibble of abbreviations coming from FRAGSTATS
#'
#' A single tibble for every abbreviation of every metric that is
#' reimplemented in landscapemetrics and its corresponding full name
#' in the literature.
#'
#' Can be used after calculating the metric(s) with a join to have
#' a more readable results tibble or for visualizing your results.
#'
#' @examples
#' patch_area <- lsm_p_area(landscape)
#' patch_area <- merge(x = patch_area, y = lsm_abbreviations_names, by = c("level", "metric"))
#'
#' @format A tibble object.
"lsm_abbreviations_names"
