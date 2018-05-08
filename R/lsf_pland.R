#' Percentage of landscape (class level)
#'
#' @description Percentage of landscape (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' library(NLMR)
#' landscape <- nlm_randomcluster(ncol = 30, nrow = 30,
#'                                p = 0.4, ai = c(0.25, 0.25, 0.5))
#' landscape_pr(landscape)
#'
#' @aliases landscape_pr
#' @rdname landscape_pr
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export


lsf_pland <- function(landscape, scale = 'class') {

    number_cells <- raster::ncell(landscape)
    resolution <- prod(raster::res(landscape))
    area <- number_cells * resolution

    if(scale == 'class'){
        if (raster::nlayers(landscape) == 1) {
            percentage <- landscape %>%
                raster::values() %>%
                table() %>%
                purrr::map_dfc(function(x) {((x * resolution) / area) * 100})

        }

        else {
            percentage <- purrr::map_dfr(1:raster::nlayers(landscape), function(x){
                raster::values(landscape[[x]]) %>%
                    table() %>%
                    purrr::map_dfc(function(x) {((x * resolution) / area) * 100})})
        }
    }

    return(percentage)

}
