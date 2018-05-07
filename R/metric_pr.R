#' Patch richness
#'
#' @description Number of patch types (classes) in the landscape
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' library(NLMR)
#' landscape <- nlm_randomcluster(ncol = 30, nrow = 30,
#'                                p = 0.4, ai = c(0.25, 0.25, 0.5))
#' metric_pr(landscape)
#'
#' @aliases metric_pr
#' @rdname metric_pr
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export


metric_pr <- function(landscape) {

   richness <-  length(raster::unique(landscape))

   return(richness)

}
