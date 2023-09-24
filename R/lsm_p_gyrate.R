#' GYRATE (patch level)
#'
#' @description Radius of Gyration (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param cell_center If true, the coordinates of the centroid are forced to be
#' a cell center within the patch.
#'
#' @details
#' \deqn{GYRATE = \sum \limits_{r = 1}^{z} \frac{h_{ijr}} {z}}
#' where \eqn{h_{ijr}} is the distance from each cell to the centroid of the
#' patch and \eqn{z} is the number of cells.
#'
#' GYRATE is an 'Area and edge metric'. The distance from each cell to the
#' patch centroid is based on cell center to centroid distances. The metric
#' characterises both the patch area and compactness.
#'
#' If `cell_center = TRUE` some patches might have several possible cell-center
#' centroids. In this case, the gyrate index is based on the mean distance of all
#' cells to all possible cell-center centroids.
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_p_gyrate(landscape)
#'
#' @aliases lsm_p_gyrate
#' @rdname lsm_p_gyrate
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Keitt, T. H., Urban, D. L., & Milne, B. T. 1997. Detecting critical scales
#' in fragmented landscapes. Conservation ecology, 1(1).
#'
#' @export
lsm_p_gyrate <- function(landscape, directions = 8,
                                     cell_center = FALSE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_gyrate_calc,
                     directions = directions,
                     cell_center = cell_center)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_gyrate_calc <- function(landscape, directions, cell_center,
                              points = NULL, extras = NULL) {

    # conver to matrix
    if (!inherits(x = landscape, what = "matrix")) {

        # get coordinates and values of all cells
        points <- raster_to_points(landscape)[, 2:4]

        # convert to matrix
        landscape <- terra::as.matrix(landscape, wide = TRUE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "gyrate",
                              value = as.double(NA)))
    }

    # get unique class id
    if (!is.null(extras$classes)){
        classes <- extras$classes
        class_patches <- extras$class_patches
    } else {
        classes <- get_unique_values_int(landscape, verbose = FALSE)
        class_patches <- get_class_patches(landscape, classes, directions)
    }

    gyrate <- do.call(rbind,
                      lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- class_patches[[as.character(patches_class)]]

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
        centroid <- stats::aggregate(points[, c(1, 2)],
                                     by = list(id = points[, 3]),
                                     FUN = mean)

        # create full data set with raster-points and patch centroids
        full_data <- merge(x = points, y = centroid, by = "id",
                           suffixes = c("","_centroid"))

        # calculate distance from each cell center to centroid
        full_data$dist <- sqrt((full_data$x - full_data$x_centroid) ^ 2 +
                                   (full_data$y - full_data$y_centroid) ^ 2)

        # force centroid to be within patch
        if (cell_center) {

            # which cell has the shortest distance to centroid
            centroid <- do.call(rbind, by(data = full_data,
                                          INDICES = full_data[, 1],
                                          FUN = function(x)
                                              x[x$dist == min(x$dist), ]))[, c(1, 2, 3)]

            # create full data set with raster-points and patch centroids
            full_data <- merge(x = points, y = centroid, by = "id",
                               suffixes = c("","_centroid"))

            # calculate distance from each cell center to centroid
            full_data$dist <- sqrt((full_data$x - full_data$x_centroid) ^ 2 +
                                       (full_data$y - full_data$y_centroid) ^ 2)
        }

        # mean distance for each patch
        gyrate_class <- stats::setNames(stats::aggregate(x = full_data[, 6],
                                                         by = list(full_data[, 1]),
                                                         FUN = mean),
                                        nm = c("id", "dist"))

        data.frame(class = as.integer(patches_class),
                   value = as.double(gyrate_class$dist))
        })
    )

    tibble::tibble(level = "patch",
                   class = as.integer(gyrate$class),
                   id = as.integer(seq_len(nrow(gyrate))),
                   metric = "gyrate",
                   value = as.double(gyrate$value))

}
