#' Total class area (class level)
#'
#' @description Total area of class (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_ta(landscape)
#'
#' @aliases lsm_ta
#' @rdname lsm_ta
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export


lsm_ta <- function(landscape, scale = 'class') {

    number_cells <- raster::ncell(landscape)
    resolution <- prod(raster::res(landscape))
    area <- number_cells * resolution

    if(scale == 'class'){
        if (raster::nlayers(landscape) == 1) {
            total_area <- landscape %>%
                raster::values() %>%
                table() %>%
                purrr::map_dfc(function(x) {x * resolution})

        }

        else {
            total_area <- purrr::map_dfr(1:raster::nlayers(landscape), function(x){
                raster::values(landscape[[x]]) %>%
                    table() %>%
                    purrr::map_dfc(function(x) {x * resolution})})
        }
    }

    return(percentage)

}
