#' Patch richness
#'
#' @description Number of patch types (classes) in the landscape
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_l_pr(landscape)
#'
#' @aliases lsm_l_pr
#' @rdname lsm_l_pr
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export

lsm_l_pr <- function(landscape) {

     if (raster::nlayers(landscape) == 1) {
        richness <- length(raster::unique(landscape))
     } else {
        # map over each layer of the rasterstack/brick
        richness_vector <-
           purrr::map_dbl(1:raster::nlayers(landscape),
                          function(x)
                             length(raster::unique(landscape[[x]])))

        # convert to tibble
        richness <-
        tibble::tibble(
              Layer = purrr::map_chr(seq_along(richness_vector),
                                     function(x)
                                        paste("Layer_", x)),
              Richness = richness_vector
        )
     }

   return(richness)

}
