#' Padding
#'
#' @description Adding padding to raster
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return raster
#'
#' @examples
#' padding(landscape)
#' padding(landscape_stack)
#'
#' @aliases padding
#' @rdname padding
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
padding <- function(landscape, padding_value) UseMethod("padding")

#' @name padding
#' @export
padding.RasterLayer <- function(landscape, padding_value = NA) {
    padding_internal(landscape, padding_value)
}

#' @name padding
#' @export
padding.RasterStack <- function(landscape) {
    purrr::map(raster::as.list(landscape), padding_internal, padding_value = padding_value)
}

#' @name padding
#' @export
padding.RasterBrick <- function(landscape) {
    purrr::map(raster::as.list(landscape), padding_internal, padding_value = padding_value)
}

#' @name padding
#' @export
padding.list <- function(landscape) {
    purrr::map(landscape, padding_internal, padding_value = padding_value)
}

padding_internal <- function(landscape, padding_value){
    landscape_matrix <- raster::as.matrix(landscape)

    landscape_matrix <- rbind(padding_value, landscape_matrix, padding_value, deparse.level = 0)
    landscape_matrix <- cbind(padding_value, landscape_matrix, padding_value, deparse.level = 0)

    landscape_padded <- raster::raster(landscape_matrix)

    return(landscape_padded)

}
