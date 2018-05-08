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
#' @aliases lsm_rpr
#' @rdname lsm_rpr
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export

lsm_rpr <- function(landscape, classes_max = NULL,
                    scale = "landscape") {

    if (scale == "landscape") {
        if (raster::nlayers(landscape) == 1) {
            richness <- length(raster::unique(landscape))
            if(is.null(classes_max)){classes_max <- richness}
            richness_relative <- richness / classes_max * 100
        } else {
            # map over each layer of the rasterstack/brick
            richness_relative <- purrr::map_dfr(1:raster::nlayers(landscape), function(x){
                if(is.null(classes_max)){classes_max <- length(raster::unique(landscape[[x]]))}
                tibble::tibble(
                    Layer = paste0('Layer_', x),
                    rpr = length(raster::unique(landscape[[x]])) / classes_max * 100)
            })
        }

    }

    return(richness_relative)

}
