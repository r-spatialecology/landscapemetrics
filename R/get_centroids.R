#' get_centroids
#'
#' @description Centroid of patches
#'
#' @param landscape Raster* Layer, Stack, Brick, stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param cell_center If true, the coordinates of the centroid are forced to be
#' a cell center within the patch.
#' @param return_sp If true, a SpatialPointsDataFrame is returned.
#' @param verbose Print warning messages
#'
#' @details
#' Get the coordinates of the centroid of each patch. The centroid is by default
#' defined as the mean location of all cell centers. To force the centroid to be
#' located within each patch, use the `cell_center` argument. In this case, the
#' centroid is defined as the cell center that is the closest to the mean location.
#'
#' @examples
#' # get centroid location
#' get_centroids(landscape)
#'
#' @aliases get_centroids
#' @rdname get_centroids
#'
#' @export
get_centroids <- function(landscape, directions, cell_center, return_sp, verbose) UseMethod("get_centroids")

#' @name get_centroids
#' @export
get_centroids.RasterLayer <- function(landscape, directions = 8,
                                     cell_center = FALSE,
                                     return_sp = FALSE,
                                     verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_centroids_calc,
                     directions = directions,
                     cell_center = cell_center,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- tibble::add_column(result, layer, .before = TRUE)

    if (return_sp) {

        result <-  sp::SpatialPointsDataFrame(coords = result[, c(5:6)],
                                              data = result[, c(1:4)],
                                              proj4string = raster::crs(landscape))
    }

    return(result)
}

#' @name get_centroids
#' @export
get_centroids.RasterStack <- function(landscape, directions = 8,
                                     cell_center = FALSE,
                                     return_sp = FALSE,
                                     verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_centroids_calc,
                     directions = directions,
                     cell_center = cell_center,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- tibble::add_column(result, layer, .before = TRUE)

    return(result)
}

#' @name get_centroids
#' @export
get_centroids.RasterBrick <- function(landscape, directions = 8,
                                     cell_center = FALSE,
                                     return_sp = FALSE,
                                     verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_centroids_calc,
                     directions = directions,
                     cell_center = cell_center,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- tibble::add_column(result, layer, .before = TRUE)

    if (return_sp) {

        result <-  sp::SpatialPointsDataFrame(coords = result[, c(5:6)],
                                              data = result[, c(1:4)],
                                              proj4string = raster::crs(landscape))
    }

    return(result)
}

#' @name get_centroids
#' @export
get_centroids.stars <- function(landscape, directions = 8,
                               cell_center = FALSE,
                               return_sp = FALSE,
                               verbose = TRUE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_centroids_calc,
                     directions = directions,
                     cell_center = cell_center,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- tibble::add_column(result, layer, .before = TRUE)

    if (return_sp) {

        result <-  sp::SpatialPointsDataFrame(coords = result[, c(5:6)],
                                              data = result[, c(1:4)],
                                              proj4string = raster::crs(landscape))
    }

    return(result)
}

#' @name get_centroids
#' @export
get_centroids.list <- function(landscape, directions = 8,
                              cell_center = FALSE,
                              return_sp = FALSE,
                              verbose = TRUE) {

    result <- lapply(X = landscape,
                     FUN = get_centroids_calc,
                     directions = directions,
                     cell_center = cell_center,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- tibble::add_column(result, layer, .before = TRUE)

    if (return_sp) {

        result <-  sp::SpatialPointsDataFrame(coords = result[, c(5:6)],
                                              data = result[, c(1:4)],
                                              proj4string = raster::crs(landscape))
    }

    return(result)
}

get_centroids_calc <- function(landscape, directions, cell_center, verbose) {

    # conver to matrix
    if (!inherits(x = landscape, what = "matrix")) {

        # get coordinates and values of all cells
        points <- raster_to_points(landscape)[, 2:4]

        # convert to matrix
        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {

        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              y = as.double(NA),
                              y = as.double(NA)))
    }

    # get uniuqe class id
    classes <- get_unique_values(landscape)[[1]]

    centroid <- do.call(rbind,
                        lapply(classes, function(patches_class) {
                            # get connected patches
                            landscape_labeled <- get_patches(landscape,
                                                             class = patches_class,
                                                             directions = directions,
                                                             return_raster = FALSE)[[1]]

                            # transpose to get same direction of ID
                            landscape_labeled <- t(landscape_labeled)

                            # get coordinates of current class
                            points <- matrix(points[which(!is.na(landscape_labeled)), ],
                                             ncol = 3)

                            # set ID from class ID to unique patch ID
                            points[, 3] <- landscape_labeled[!is.na(landscape_labeled)]

                            # # conver to tibble
                            points <- stats::setNames(object = data.frame(points),
                                                      nm = c("x", "y", "id"))

                            # calcuale the centroid of each patch (mean of all coords)
                            centroid_temp <- stats::aggregate(points[, c(1, 2)],
                                                              by = list(id = points[, 3]),
                                                              FUN = mean)

                            # force centroid to be within patch
                            if (cell_center) {

                                # create full data set with raster-points and patch centroids
                                full_data <- merge(x = points, y = centroid_temp, by = "id",
                                                   suffixes = c("","_centroid"))

                                # calculate distance from each cell center to centroid
                                full_data$dist <- sqrt((full_data$x - full_data$x_centroid) ^ 2 +
                                                           (full_data$y - full_data$y_centroid) ^ 2)

                                # which cell has the shortest distance to centroid
                                centroid_temp <-
                                    do.call(rbind,
                                            by(data = full_data,
                                               INDICES = full_data[, 1],
                                               FUN = function(x) {
                                                   x[x$dist == min(x$dist), ][, c(1, 2, 3)]}))
                            }

                            # return current class id and coords
                            data.frame(class = patches_class,
                                       id = centroid_temp$id,
                                       x = centroid_temp$x,
                                       y = centroid_temp$y)
                        })
    )

    # get number of total patches to construct id seq
    np <- lsm_l_np_calc(landscape, directions = directions)[[1, 5]]

    # check how often different combinations of class-id are present
    times <- as.numeric(t(table(centroid[, c(1, 2)])))

    # remove all 0 cases
    times <- times[which(times != 0)]

    # repeat each id (# patches) where times is the number of often the class-id
    # combination is present
    id <- rep(seq_len(np), times = times[times != 0])

    # return warning if any patch has several centroids
    if (verbose) {

        if (any(times != 1)) {
           warning("For some patches several cell centers are returned as centroid.",
                   call. = FALSE)
        }
    }

    tibble::tibble(level = "patch",
                   class = as.integer(centroid$class),
                   id = as.integer(id),
                   x = as.double(centroid$x),
                   y = as.double(centroid$y))
}
