#' get_circumscribingcircle
#'
#' @description Calculates the diameter of the smallest circumscribing circle around
#' patches in a landscape. The patches must have a unique ID column.
#'
#' @param landscape RasterLayer or matrix (with x, y, id columns)
#' @param resolution_x Resolution of the landscape (only needed if matrix as input is used)
#' @param resolution_y Resolution of the landscape (only needed if matrix as input is used)
#'
#' @details
#' Fast and memory safe Rcpp implementation for calculating maximum euclidean distances between
#' cells of the same class in a raster or matrix. Uses the edge boundary of cells,
#' not the cell center. Using the edge boundary and the maximum distance between
#' the 4 cell corners around each cell center of the patch derives in the diameter of
#' the smallest circumscribing circle around a patch.
#'
#' If one uses this functions with a matrix the resolution of the underlying data must be provided.
#'
#' @references
#' Based on RCpp code of Florian Privé \email{florian.prive.21@gmail.com}
#'
#' @examples
#' # get patches for class 1 from testdata as raster
#' class_1 <- get_patches(landscape, class = 1)[[1]]
#'
#' # calculate the max distance between cell edges of each class
#' get_circumscribingcircle(class_1)
#'
#' # do the same with a 3 column matrix (x, y, id)
#' class_1_matrix <- raster::rasterToPoints(class_1)
#' get_circumscribingcircle(class_1_matrix, resolution_x = 1, resolution_y = 1)
#'
#' @aliases get_circumscribingcircle
#' @rdname get_circumscribingcircle
#'
#' @export
get_circumscribingcircle <- function(landscape, resolution_x, resolution_y) UseMethod("get_circumscribingcircle")

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.RasterLayer <- function(landscape,
                                                 resolution_x = NULL,
                                                 resolution_y = NULL) {

    points_mat <- raster::rasterToPoints(landscape)

    resolution_xy <- raster::res(landscape)
    resolution_x <- resolution_xy[[1]]
    resolution_y <- resolution_xy[[2]]

    circle <- rcpp_get_circle(points_mat,
                              resolution_x = resolution_x,
                              resolution_y = resolution_y)

    tibble::tibble(id = circle[, 1],
                   dist = circle[, 2])
}


#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.matrix <- function(landscape,
                                            resolution_x = NULL,
                                            resolution_y = NULL) {

    if ( ncol(landscape) != 3){
        stop("Coordinate matrix must have 3 (x, y, id) columns.", call. = TRUE)
    }

    if (is.null(resolution_x) || is.null(resolution_y)){
        stop("Resolution must be provided to correctly calculate the edges. ", call. = TRUE)
    }

    circle <- rcpp_get_circle(landscape,
                              resolution_x = resolution_x,
                              resolution_y = resolution_y)

    tibble::tibble(id = circle[, 1],
                   dist = circle[, 2])

}
