#' get_nearestneighbour
#'
#' @description Euclidean distance to nearest neighbour
#'
#' @param landscape RasterLayer or matrix (with x,y,id columns).
#' @param return_id If TRUE, also the patch ID of the nearest neighbour is returned.
#'
#' @details
#' Fast and memory safe Rcpp implementation for calculating the minimum Euclidean
#' distances to the nearest patch of the same class in a raster or matrix.
#' All patches need an unique ID (see \code{\link{get_patches}}).
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
#' @aliases get_nearestneighbour
#' @rdname get_nearestneighbour
#'
#' @export
get_nearestneighbour <- function(landscape, return_id) UseMethod("get_nearestneighbour")

#' @name get_nearestneighbour
#' @export
get_nearestneighbour.RasterLayer <- function(landscape, return_id = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_nearestneighbour_calc,
                     return_id = return_id)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name get_nearestneighbour
#' @export
get_nearestneighbour.RasterStack <- function(landscape, return_id = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_nearestneighbour_calc,
                     return_id = return_id)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name get_nearestneighbour
#' @export
get_nearestneighbour.RasterBrick <- function(landscape, return_id = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_nearestneighbour_calc,
                     return_id = return_id)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name get_nearestneighbour
#' @export
get_nearestneighbour.stars <- function(landscape, return_id = FALSE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_nearestneighbour_calc,
                     return_id = return_id)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name get_nearestneighbour
#' @export
get_nearestneighbour.list <- function(landscape, return_id = FALSE) {

    result <- lapply(X = landscape,
                     FUN = get_nearestneighbour_calc,
                     return_id = return_id)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

get_nearestneighbour_calc <- function(landscape, return_id) {

    points_mat <- raster_to_points(landscape, return_NA = FALSE)[, 2:4]

    ord <- order(as.matrix(points_mat)[, 1])
    num <- seq_along(ord)
    rank <- match(num, ord)

    res <- rcpp_get_nearest_neighbor(raster::as.matrix(points_mat)[ord, ])

    min_dist <- tibble::tibble(cell = num,
                               dist = res[rank, 1],
                               id_focal = points_mat[, 3],
                               id_neighbour = res[rank, 2])

    min_dist_aggr <- stats::setNames(stats::aggregate(x = min_dist$dist,
                                                      by = list(min_dist$id_focal),
                                                      FUN = min),
                                c("id", "distance"))

    if(return_id) {

        min_dist_aggr <- merge(x = min_dist_aggr, y = min_dist[, c(2, 3, 4)],
                          by.x = c("id", "distance"),
                          by.y = c("id_focal", "dist"))

        min_dist_aggr <- min_dist_aggr[!duplicated(min_dist_aggr), ]
    }

    tibble::tibble(min_dist_aggr)
}
