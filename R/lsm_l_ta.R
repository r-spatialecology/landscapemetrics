#' Total class area (landcape level)
#'
#' @description Total area (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_ta(landscape)
#'
#' @aliases lsm_l_ta
#' @rdname lsm_l_ta
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export

lsm_l_ta <- function(landscape){

    if (raster::nlayers(landscape) == 1){
        total_area <- tibble::tibble(
            layer = as.numeric(1),
            level = 'landcape',
            id = as.numeric(NA),
            metric = 'TA',
            value = raster::ncell(landscape) * prod(raster::res(landscape))
        )
    }

    else {
        total_area <- purrr::map_dfr(1:raster::nlayers(landscape), function(x){
            tibble::tibble(
                level = 'landscape',
                id = as.numeric(NA),
                metric = 'TA',
                value = raster::ncell(landscape[[x]]) * prod(raster::res(landscape[[x]]))
                           )
            }, .id = 'layer') %>%
            dplyr::mutate(layer = as.numeric(layer))
    }

    return(total_area)
}
