#' Shannon's Diversity Index
#'
#' @description Number of patch types (classes) in the landscape
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @return Value >= 0
#'
#' @examples
#' lsm_l_shdi(landscape)
#'
#' @aliases lsm_l_shdi
#' @rdname lsm_l_shdi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export

lsm_l_shdi <- function(landscape) {

    x <- raster::as.matrix(landscape)/(total <- sum(raster::as.matrix(landscape)))
    x <- -x * log(x, exp(1))
    H <- sum(x, na.rm = TRUE)

    return(H)

}

