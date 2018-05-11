#' Number of patches
#'
#' @description Number of patches
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

lsm_l_np <- function(landscape) {

    if (raster::nlayers(landscape) == 1) {
        patches <-  cclabel(landscape)
        n_patches <- sum(patches@data@max)
    } else {
        # raster stack
        patches <-  cclabel(landscape)
        n_patches <- purrr::map_dbl(seq_along(patches),
                                    function(x){
                                        sum(patches[[x]]@data@max)
                                    })


        n_patches <- tibble::tibble(
            layer = seq(1, raster::nlayers(landscape)),
            level = 'landscape',
            id = as.numeric(NA),
            metric = 'number of patches',
            value = n_patches
        )
    }

    return(n_patches)
}
