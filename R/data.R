#' Example map (random cluster neutral landscape model).
#'
#' An example map to show landscapetools functionality
#' generated with the `nlm_randomcluster()` algorithm.
#'
#' @format A raster layer object.
#' @source Simulated neutral landscape model with R. https://github.com/ropensci/NLMR/
"landscape"

#' Example maps as raster stack (random cluster neutral landscape model).
#'
#' An example rasterstack to show landscapetools functionality
#' generated with `nlm_mpd()` and `nlm_random()`.
#'
#' @format A raster stack object.
#' @source Simulated neutral landscape model with R. https://github.com/ropensci/NLMR/
"landscape_stack"

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

#' Fragstats results (patch level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on patch
#' level.
#'
#' @format A tibble object.
"fragstats_patch_landscape"
"fragstats_patch_landscapestack"
"fragstats_patch_augusta_nlcd"
"fragstats_patch_podlasie"

#' Fragstats results (class level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on class
#' level.
#'
#' @format A tibble object.
"fragstats_class_landscape"
"fragstats_class_landscapestack"
"fragstats_class_augusta_nlcd"
"fragstats_class_podlasie"


#' Fragstats results (landscape level)
#'
#' A single tibble for every spatial dataset included in landscapemetrics
#' that contains the FRAGSTAT results for every implemented metric on landscape
#' level.
#'
#' @format A tibble object.
"fragstats_landscape_landscape"
"fragstats_landscape_landscapestack"
"fragstats_landscape_augusta_nlcd"
"fragstats_landscape_podlasie"
