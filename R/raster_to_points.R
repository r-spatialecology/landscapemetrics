#' raster_to_points
#'
#' @description Raster to points
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
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
raster_to_points <- function(landscape, return_NA) UseMethod("raster_to_points")

#' @name raster_to_points
#' @export
raster_to_points.RasterLayer <- function(landscape, return_NA = TRUE){

    result <- raster_to_points_internal(landscape,
                                        return_NA = return_NA)

    result <- cbind(layer = 1, result)

    return(result)
}

#' @name raster_to_points
#' @export
raster_to_points.RasterStack <- function(landscape, return_NA = TRUE){

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), function(x) {

        xyz <- raster_to_points_internal(landscape[[x]],
                                         return_NA = return_NA)

        xyz <- cbind(layer = x, xyz)
    })

    result <- do.call(rbind, result)

    return(result)
}

#' @name raster_to_points
#' @export
raster_to_points.RasterBrick <- function(landscape, return_NA = TRUE){

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), function(x) {

        xyz <- raster_to_points_internal(landscape[[x]],
                                         return_NA = return_NA)

        xyz <- cbind(layer = x, xyz)
    })

    result <- do.call(rbind, result)

    return(result)
}

#' @name raster_to_points
#' @export
raster_to_points.stars <- function(landscape, return_NA = TRUE){

    landscape <- methods::as(landscape, "Raster")

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), function(x) {

        xyz <- raster_to_points_internal(landscape[[x]],
                                         return_NA = return_NA)

        xyz <- cbind(layer = x, xyz)
    })

    result <- do.call(rbind, result)

    return(result)
}

#' @name raster_to_points
#' @export
raster_to_points.list <- function(landscape, return_NA = TRUE){

    result <- lapply(X = seq_along(landscape), function(x) {

        xyz <- raster_to_points_internal(landscape[[x]],
                                         return_NA = return_NA)

        xyz <- cbind(layer = x, xyz)
    })

    result <- do.call(rbind, result)

    return(result)
}

raster_to_points_internal <- function(landscape, return_NA) {

    # preallocate matrix
    xyz <- matrix(data = NA,
                  nrow = raster::ncell(landscape), ncol = 3)

    # get coordinates
    xyz[, c(1,2)] <- raster::xyFromCell(landscape,
                                        cell = 1:raster::ncell(landscape))

    # add values including NA
    xyz[, 3] <- raster::getValues(landscape)

    if (!return_NA) {
        xyz <- xyz[!is.na(xyz[, 3]), ]
    }

    colnames(xyz) <- c("x", "y", "z")

    # xyz <- cbind(layer = 1, xyz)

    return(xyz)
}
