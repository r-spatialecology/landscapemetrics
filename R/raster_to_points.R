#' raster_to_points
#'
#' @description Raster to points
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Wrapper around raster::xyFromCell and raster::getValues to get raster_to_points
#' function including NA values
#'
#' @return matrix
#'
#' @examples
#' raster_to_points(landscape)
#'
#' @aliases raster_to_points
#' @rdname raster_to_points
#'
#' @keywords internal
#'
#' @export
raster_to_points <- function(landscape){

    # get coordinates
    xyz <- raster::xyFromCell(landscape, cell = 1:raster::ncell(landscape))

    # add values including NA
    xyz <- cbind(xyz, raster::getValues(landscape))

    return(xyz)
}
