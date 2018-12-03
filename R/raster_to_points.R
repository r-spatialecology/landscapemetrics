#' raster_to_points
#'
#' @description Raster to points
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param return_NA If true, NA cells are also included
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
raster_to_points <- function(landscape, return_NA = TRUE){

    # get coordinates
    xyz <- raster::xyFromCell(landscape, cell = 1:raster::ncell(landscape))

    # add values including NA
    xyz <- cbind(xyz, raster::getValues(landscape))


    if(!return_NA) {
        xyz <- xyz[!is.na(xyz[, 3]), ]
    }

    colnames(xyz) <- c("x", "y", "z")

    return(xyz)
}
