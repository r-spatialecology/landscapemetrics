#' Number of patches (class level)
#'
#' @description Number of patches (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_np(landscape)
#'
#' @aliases lsm_l_np
#' @rdname lsm_l_np
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.

# Not running at the moment

# lsm_l_np <- function(landscape) {
#
#     if (raster::nlayers(landscape) == 1) {
#         number_patches <- landscape %>%
#             spex::polygonize() %>%
#             dplyr::group_by_(value_name) %>%
#             dplyr::summarise() %>%
#             purrr::map(.$geometry, function(x) x)
#         }
#
#     else {
#         # raster stack
#     }
#
#     return(number_patches)
# }
