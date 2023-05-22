#' Example map (random cluster neutral landscape model).
#'
#' An example map to show landscapemetrics functionality
#' generated with the `nlm_randomcluster()` algorithm.
#'
#' @format A raster object.
#' @source Simulated neutral landscape model with R. https://github.com/ropensci/NLMR/
"landscape"

#' Augusta NLCD 2011
#'
#' A real landscape of area near Augusta, Georgia obtained from the National
#' Land Cover Database (NLCD)
#'
#' @format A raster object.
#' @source https://www.mrlc.gov/nlcd2011.php
#' @references Homer, C.G., Dewitz, J.A., Yang, L., Jin, S., Danielson, P., Xian, G., Coulston, J., Herold, N.D., Wickham, J.D., and Megown, K., 2015, Completion of the 2011 National Land Cover Database for the conterminous United States-Representing a decade of land cover change information. Photogrammetric Engineering and Remote Sensing, v. 81, no. 5, p. 345-354
"augusta_nlcd"

#' Podlasie ESA CCI LC
#'
#' A real landscape of the Podlasie region in Poland from the ESA CCI Land
#' Cover
#'
#' @format A raster object.
#' @source http://maps.elie.ucl.ac.be/CCI/viewer/
"podlasie_ccilc"


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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' patch_area <- lsm_p_area(landscape)
#' patch_area <- merge(x = patch_area, y = lsm_abbreviations_names, by = c("level", "metric"))
#'
#' @format A tibble object.
"lsm_abbreviations_names"
