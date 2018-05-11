#' Percentage of landscape (class level)
#'
#' @description Percentage of landscape (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_pland(landscape)
#'
#' @aliases lsm_c_pland
#' @rdname lsm_c_pland
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_pland <- function(landscape) {

    area <- raster::ncell(landscape) * prod(raster::res(landscape))

    if (raster::nlayers(landscape) == 1) {
        percentage <- landscape %>%
            raster::values() %>%
            table() %>%
            purrr::map2_dfr(.x = ., .y = 1:length(.), .f = function(x, y) {
                tibble::tibble(
                    layer = as.integer(1),
                    level = 'class',
                    id = as.integer(y),
                    metric = 'percentage of landscape',
                    value = x * prod(raster::res(landscape)) / area * 100
                )
            })
        }

    else {
        percentage <- purrr::map_dfr(seq_len(raster::nlayers(landscape)), function(x){
            raster::values(landscape[[x]]) %>%
                table() %>%
                purrr::map2_dfr(.x = ., .y = 1:length(.), .f = function(x, y) {
                    tibble::tibble(
                        level = 'class',
                        id = as.integer(y),
                        metric = 'percentage of landscape',
                        value = x * prod(raster::res(landscape)) / area * 100
                    )
                })
            },  .id = 'layer') %>%
            dplyr::mutate(layer = as.integer(layer))
        }

    return(percentage)
}
