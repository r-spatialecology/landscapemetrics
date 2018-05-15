#' Shannon's Evenness Index
#'
#' @description Evenness of patch types (classes) in the landscape
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @return tibble
#'
#' @examples
#' lsm_l_shei(landscape)
#' lsm_l_shei(landscape_stack)
#' lsm_l_shei(as.list(landscape_stack))
#'
#' @aliases lsm_l_shei
#' @rdname lsm_l_shei
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export

lsm_l_shei <- function(landscape) {

    area <- raster::ncell(landscape)

    if (raster::nlayers(landscape) == 1){
        p <- landscape %>%
            raster::values() %>%
            table() / area

        E <- tibble::tibble(
            layer = as.numeric(1),
            level = 'landscape',
            id = as.numeric(NA),
            metric = 'Shannon evenness',
            value = sum(-p * log(p, exp(1)), na.rm = TRUE) / log(length(p), exp(1))
        )
    }

    else {
        E <- purrr::map_dfr(1:raster::nlayers(landscape), function(x){
            p <- landscape[[x]] %>%
                raster::values() %>%
                table() / area

            tibble::tibble(
                level = 'landscape',
                id = as.numeric(NA),
                metric = 'Shannon evenness',
                value = sum(-p * log(p, exp(1)), na.rm = TRUE) / log(length(p), exp(1))
            )
        }, .id = 'layer') %>%
            dplyr::mutate(layer = as.numeric(layer))
    }


    return(E)

}

