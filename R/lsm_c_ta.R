#' Total class area (class level)
#'
#' @description Total area of class (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ta(landscape)
#'
#' @aliases lsm_c_ta
#' @rdname lsm_c_ta
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export


lsm_c_ta <- function(landscape) {

    area <- raster::ncell(landscape) * prod(raster::res(landscape))

    if (raster::nlayers(landscape) == 1) {
        total_area <- landscape %>%
            raster::values() %>%
            table() %>%
            purrr::map2_dfr(.x = ., .y = 1:length(.), function(x, y) {
                tibble::tibble(
                    layer = as.numeric(1),
                    level = 'class',
                    id = as.numeric(y),
                    metric = 'total area',
                    value = x * prod(raster::res(landscape))
                )
            })
        }

    else {
        total_area <- purrr::map_dfr(1:raster::nlayers(landscape), function(x){
            raster::values(landscape[[x]]) %>%
                table() %>%
                purrr::map2_dfr(.x = ., .y = 1:length(.), .f = function(x, y) {
                    tibble::tibble(
                        level = 'class',
                        id = as.numeric(y),
                        metric = 'total area',
                        value = x * prod(raster::res(landscape))
                    )
                })
            },  .id = 'layer') %>%
            dplyr::mutate(layer = as.numeric(layer))
        }

    return(total_area)
}
