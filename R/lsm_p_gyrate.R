#' GYRATE (patch level)
#'
#' @description Radius of Gyration (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{GYRATE = \sum \limits_{r = 1}^{z} \frac{h_{ijr}} {z}}
#' where \eqn{h_{ijr}} is the distance from each cell to the centroid of the
#' patch and \eqn{z} is the number of cells.
#'
#' GYRATE is an 'Area and edge metric'. The distance from each cell to the
#' patch
#' centroid is based on cell center-to-cell center distances. The metrics
#' characterises both the patch area and compactness.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE >= 0}
#' \subsection{Behaviour}{Approaches GYRATE = 0 if patch is a single cell.
#' Increases, without limit, when only one patch is present.}
#'
#' @seealso
#' \code{\link{lsm_c_gyrate_mn}},
#' \code{\link{lsm_c_gyrate_sd}},
#' \code{\link{lsm_c_gyrate_cv}}, \cr
#' \code{\link{lsm_l_gyrate_mn}},
#' \code{\link{lsm_l_gyrate_sd}},
#' \code{\link{lsm_l_gyrate_cv}}
#' @return tibble
#'
#' @examples
#' lsm_p_gyrate(landscape)
#'
#' @aliases lsm_p_gyrate
#' @rdname lsm_p_gyrate
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Keitt, T. H., Urban, D. L., & Milne, B. T. 1997. Detecting critical scales
#' in fragmented landscapes. Conservation ecology, 1(1).
#'
#' @export
lsm_p_gyrate <- function(landscape, directions) UseMethod("lsm_p_gyrate")

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_gyrate_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_gyrate_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_gyrate_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")


    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_gyrate_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_p_gyrate_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_gyrate_calc <- function(landscape, directions,
                              points = NULL) {

    # conver to matrix
    if (class(landscape) != "matrix") {

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
                              metric = "gyrate",
                              value = as.double(NA)))
    }

    # get uniuqe class id
    classes <- get_unique_values(landscape)[[1]]

    gyrate <- do.call(rbind,
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

        # conver to tibble -> do we still need to do this?
        points <- tibble::as_tibble(points)
        names(points) <- c("x", "y", "id")

        # calcuale the centroid of each patch (mean of all coords)
        centroid <- stats::aggregate(points[, c(1, 2)],
                                     by = list(id = points$id),
                                     FUN = mean)

        # create full data set with raster-points and patch centroids
        full_data <- tibble::as_tibble(merge(x = points, y = centroid, by = "id",
                           suffixes = c("","_centroid")))

        # calculate distance from each cell center to centroid
        full_data$dist <- sqrt((full_data$x - full_data$x_centroid) ^ 2 +
                                   (full_data$y - full_data$y_centroid) ^ 2)

        # mean distance for each patch
        gyrate_class <- stats::aggregate(x = full_data[, 6],
                                         by = full_data[, 1],
                                         FUN = mean)

        tibble::tibble(class = as.integer(patches_class),
                       value = as.double(gyrate_class$dist))
        })
    )

    tibble::tibble(level = "patch",
                   class = as.integer(gyrate$class),
                   id = as.integer(seq_len(nrow(gyrate))),
                   metric = "gyrate",
                   value = as.double(gyrate$value))

}
