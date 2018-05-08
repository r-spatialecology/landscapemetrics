#' Shannon's Diversity Index
#'
#' @description Number of patch types (classes) in the landscape
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param scale xxx
#' @return Value >= 0
#'
#' @examples
#' lsm_shdi(landscape)
#'
#' @aliases lsm_shdi
#' @rdname lsm_shdi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export

lsm_shdi <- function(landscape,
                     scale = "landscape") {

    x <- raster::as.matrix(landscape)/(total <- sum(raster::as.matrix(landscape)))
    x <- -x * log(x, exp(1))
    H <- sum(x, na.rm = TRUE)

    return(H)

}

