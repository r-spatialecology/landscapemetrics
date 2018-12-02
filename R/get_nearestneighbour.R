#' get_nearestneighbour
#'
#' @description Euclidean distance to nearest neighbour
#'
#' @param landscape RasterLayer or matrix (with x,y,id columns)
#'
#' @details
#' Fast and memory safe Rcpp implementation for calculating the minimum Euclidean
#' distances to the nearest patch of the same class in a raster or matrix. All patches need an unique
#' ID (see \code{\link{get_patches}}).
#'
#' @references
#' Based on RCpp code of Florian Privé \email{florian.prive.21@gmail.com}
#'
#' @examples
#' # get patches for class 1 from testdata as raster
#' class_1 <- get_patches(landscape,1)[[1]]
#'
#' # calculate the distance between patches
#' get_nearestneighbour(class_1)
#'
#' # do the same with a 3 column matrix (x, y, id)
#' class_1_matrix <- raster::rasterToPoints(class_1)
#' get_nearestneighbour(class_1_matrix)
#'
#' @aliases get_nearestneighbour
#' @rdname get_nearestneighbour
#'
#' @export
get_nearestneighbour <- function(landscape) UseMethod("get_nearestneighbour")

#' @name get_nearestneighbour
#' @export
get_nearestneighbour.RasterLayer <- function(landscape) {

    points_mat <- raster::rasterToPoints(landscape)

    ord <- order(as.matrix(points_mat)[, 1])
    num <- seq_along(ord)
    rank <- match(num, ord)

    res <- rcpp_get_nearest_neighbor(raster::as.matrix(points_mat)[ord, ])

    min_dist <- unname(cbind(num, res[rank], as.matrix(points_mat)[, 3]))

    tbl <- tibble::tibble(cell = min_dist[, 1],
                          dist = min_dist[, 2],
                          id = min_dist[, 3])

    dplyr::summarise(dplyr::group_by(tbl, id = id), distance = min(dist))
}


#' @name get_nearestneighbour
#' @export
get_nearestneighbour.matrix <- function(landscape) {

    if ( ncol(landscape) != 3){
        stop("Coordinate matrix must have 3 (x,y,id) columns.", call. = TRUE)
    }

    ord <- order(as.matrix(landscape)[, 1])
    num <- seq_along(ord)
    rank <- match(num, ord)

    res <- rcpp_get_nearest_neighbor(raster::as.matrix(landscape)[ord, ])

    min_dist <- unname(cbind(num, res[rank], as.matrix(landscape)[, 3]))

    tbl <- tibble::tibble(cell = min_dist[, 1],
                          dist = min_dist[, 2],
                          id = min_dist[, 3])

    dplyr::summarise(dplyr::group_by(tbl, id = id), distance = min(dist))
}
