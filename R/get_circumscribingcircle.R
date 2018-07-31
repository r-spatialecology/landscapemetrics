#' get_circumscribingcircle
#'
#' @description Calculates the diameter of the smallest circumscribing circle around patches in a landscape.
#'
#' @param landscape RasterLayer or matrix (with x,y,id columns)
#' @param resolution Resolution of the landscape (only needed if matrix as input is used)
#'
#' @details
#' Fast and memory safe Rcpp implementation for calcuting maximum euclidian distances between
#' cells of the same class in a raster or matrix. Uses the edge boundary of cells,
#' not the cell center. Using the edge boundary and the maximum distance between
#' the 4 cell corners around each cell center of the patch derives in the diameter of
#' the smallest circumscribing circle around a patch.
#'
#' If one uses this functions with a matrix the resolution of the underlying data must be provided.#'
#'
#' @examples
#' # get patches for class 1 from testdata as raster
#' class_1 <- get_patches(landscape,1)[[1]]
#'
#' # calculate the max distance between cell edges of each class
#' get_circumscribingcircle(class_1)
#'
#' # do the same with a 3 column matrix (x,y,id)
#' class_1_matrix <- raster::rasterToPoints(class_1)
#' get_circumscribingcircle(class_1_matrix, 1)
#'
#' @aliases get_circumscribingcircle
#' @rdname get_circumscribingcircle
#'
#' @export
get_circumscribingcircle <- function(landscape, resolution) UseMethod("get_circumscribingcircle")

#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.RasterLayer <- function(landscape, resolution) {
    points_mat <- raster::rasterToPoints(landscape)

    resol <- raster::res(landscape)

    circle <- rcpp_get_circle(points_mat, resolution = prod(resol))

    tibble::tibble(id = circle[, 1],
                   dist = circle[, 2])
}


#' @name get_circumscribingcircle
#' @export
get_circumscribingcircle.matrix <- function(landscape, resolution = NULL) {

    if ( ncol(landscape) != 3){
        stop("Coordinate matrix must have 3 (x,y,id) columns.", call. = TRUE)
    }

    if (is.null(resolution)){
        stop("Resolution must be provided to correctly calculate the edges. ", call. = TRUE)
    }

    circle <- rcpp_get_circle(landscape, resolution = resolution)

    tibble::tibble(id = circle[, 1],
                   dist = circle[, 2])

}
