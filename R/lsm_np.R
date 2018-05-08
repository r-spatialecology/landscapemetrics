#' Number of patches (class level)
#'
#' @description Number of patches (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' library(NLMR)
#' landscape <- nlm_randomcluster(ncol = 30, nrow = 30,
#'                                p = 0.4, ai = c(0.25, 0.25, 0.5))
#' lsm_pland(landscape)
#'
#' @aliases lsm_np
#' @rdname lsm_np
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export

# Not running

lsm_np <- function(landscape, scale = 'class') {

    if(scale == 'class'){
        if (raster::nlayers(landscape) == 1) {
            number_patches <- landscape %>%
                spex::polygonize() %>%
                dplyr::group_by_(value_name) %>%
                dplyr::summarise() %>%
                purrr::map(.$geometry, function(x) x)

        }

        else {

        }
    }

    return(number_patches)

}
