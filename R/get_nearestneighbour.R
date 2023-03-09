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
#' All patches need an unique ID (see \code{\link{get_patches}}). Please be aware
#' that the patch ID is not identical to the patch ID of all metric functions (lsm_).
#' If `return_ID = TRUE`, for some focal patches several nearest neighbour patches
#' might be returned.
#'
#' @references
#' Based on RCpp code of Florian Privé \email{florian.prive.21@gmail.com}
#'
#' @examples
#' # get patches for class 1
#' class_1 <- get_patches(landscape, class = 2)[[1]][[1]]
#'
#' # calculate the distance between patches
#' get_nearestneighbour(class_1)
#' get_nearestneighbour(class_1, return_id = TRUE)
#'
#' @aliases get_nearestneighbour
#' @rdname get_nearestneighbour
#'
#' @export
get_nearestneighbour <- function(landscape, return_id = FALSE) {

    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = get_nearestneighbour_calc,
                     return_id = return_id)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)

}

get_nearestneighbour_calc <- function(landscape, return_id,
                                      points = NULL) {

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {

        # get coordinates and values of all cells
        points <- raster_to_points(landscape)[, 2:4]

        # convert to matrix
        landscape <- terra::as.matrix(landscape, wide = TRUE)
    }

    # get edge cells because only they are important for ENN
    class_boundaries <- get_boundaries_calc(landscape,
                                            consider_boundary = FALSE,
                                            edge_depth = 1,
                                            as_NA = TRUE,
                                            patch_id = TRUE)

    # transpose to get same direction of ID
    class_boundaries <- t(class_boundaries)

    # get coordinates of current class
    points <- points[which(!is.na(class_boundaries)), ]

    # set ID from class ID to unique patch ID
    points[, 3] <- class_boundaries[!is.na(class_boundaries)]

    ord <- order(as.matrix(points)[, 1])
    num <- seq_along(ord)
    rank <- match(num, ord)

    res <- rcpp_get_nearest_neighbor(terra::as.matrix(points, wide= TRUE)[ord, ])

    min_dist <- tibble::tibble(cell = num,
                               dist = res[rank, 1],
                               id_focal = points[, 3],
                               id_neighbour = res[rank, 2])

    min_dist_aggr <- stats::setNames(stats::aggregate(x = min_dist$dist,
                                                      by = list(min_dist$id_focal),
                                                      FUN = min),
                                     c("id", "dist"))

    if (return_id) {

        min_dist_aggr <- merge(x = min_dist_aggr, y = min_dist[, c(2, 3, 4)],
                               by.x = c("id", "dist"),
                               by.y = c("id_focal", "dist"),
                               sort = FALSE)

        min_dist_aggr <- min_dist_aggr[!duplicated(min_dist_aggr), ]
    }

    tibble::tibble(min_dist_aggr)
}
