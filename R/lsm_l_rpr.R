#' Relative patch ritchness
#'
#' @description Number of different classes divided by (potential) maximum number of classes
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param classes_max Maximum number of classes
#' @param scale xxx
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_rpr(landscape)
#'
#' @aliases lsm_l_rpr
#' @rdname lsm_l_rpr
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export

lsm_l_rpr <- function(landscape, classes_max = NULL) {

    if (raster::nlayers(landscape) == 1) {
        if(is.null(classes_max)){classes_max <- richness}
        richness_relative <- tibble::tibble(
            layer = as.numeric(1),
            level = 'landscape',
            id = as.numeric(NA),
            metric = 'RPR',
            value = length(raster::unique(landscape)) / classes_max * 100
            )
    }

    else {
        # map over each layer of the rasterstack/brick
        richness_relative <- purrr::map_dfr(1:raster::nlayers(landscape), function(x){
            if(is.null(classes_max)){classes_max <- length(raster::unique(landscape[[x]]))}
            richness_relative <- tibble::tibble(
                level = 'landscape',
                id = as.numeric(NA),
                metric = 'RPR',
                value = length(raster::unique(landscape[[x]])) / classes_max * 100
                )
        }, .id = 'layer') %>%
        dplyr::mutate(layer = as.numeric(layer))
    }

    return(richness_relative)
}


