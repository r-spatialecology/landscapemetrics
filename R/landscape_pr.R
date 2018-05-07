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


landscape_pr <- function(landscape) {

   if (raster::nlayers(landscape) == 1) {
       richness <- length(raster::unique(landscape))
   } else {

       # map over each layer of the rasterstack/brick
       richness_vector <- purrr::map_dbl(1:raster::nlayers(landscape),
                           function(x) length(raster::unique(landscape[[x]])))

       # convert to tibble
       richness <- tibble::tibble(Layer = purrr::map_chr(seq_along(richness_vector),
                          function(x) paste("Layer_", x)),
              Richness = richness_vector)
       }

   return(richness)

}










