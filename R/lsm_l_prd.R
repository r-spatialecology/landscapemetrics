#' Patch richness density
#'
#' @description Number of patch types (classes) in the landscape divided by the total
#' landscape area
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param scale xxx
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_prd(landscape)
#'
#' @aliases lsm_prd
#' @rdname lsm_prd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export

lsm_l_prd <- function(landscape) {

    total_area <- raster::ncell(landscape) * prod(raster::res(landscape))

    if (raster::nlayers(landscape) == 1) {
        richness <- length(raster::unique(landscape))
        richness_density <- tibble::tibble(
            layer = as.numeric(1),
            level = 'landscape',
            id = as.numeric(NA),
            metric = 'patch richness density',
            value = richness / total_area * 100
            )

    } else {
        # map over each layer of the rasterstack/brick
        richness_density <- purrr::map_dfr(1:raster::nlayers(landscape), function(x){
            tibble::tibble(
                level = 'landscape',
                id = as.numeric(NA),
                metric = 'patch richness density',
                value = length(raster::unique(landscape[[x]])) / total_area * 100
                )
            }, .id = 'layer')
    }

    return(richness_density)
}
