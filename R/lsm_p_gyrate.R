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

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_gyrate_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_gyrate_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")


    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_gyrate_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_gyrate
#' @export
lsm_p_gyrate.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_p_gyrate_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_p_gyrate_calc <- function(landscape, directions) {

    # get uniuqe class id
    classes <- get_unique_values(landscape)[[1]]

    gyrate <- lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- get_patches(landscape,
                                         class = patches_class,
                                         directions = directions)[[1]]

        # convert cells to points
        points_class <- tibble::as.tibble(raster::rasterToPoints(landscape_labeled))
        names(points_class) <- c("x", "y", "id")

        # calcuale the centroid of each patch (mean of all coords)
        centroid <- dplyr::summarise(dplyr::group_by(points_class, id),
                                     x_centroid = mean(x),
                                     y_centroid = mean(y))


        # create full data set with raster-points and patch centroids
        full_data <- dplyr::left_join(x = points_class, y = centroid, by = "id")

        # calculate distance from each cell center to centroid and take mean
        gyrate_class <- dplyr::summarise(
            dplyr::group_by(
                dplyr::mutate(full_data, dist = sqrt((x - x_centroid) ^ 2 + (y - y_centroid) ^ 2)),
                id),
            value = mean(dist)
            )

        tibble::tibble(class = as.integer(patches_class),
                       value = as.double(gyrate_class$value))
    })

    gyrate <- dplyr::bind_rows(gyrate)

    tibble::tibble(level = "patch",
                   class = as.integer(gyrate$class),
                   id = as.integer(seq_len(nrow(gyrate))),
                   metric = "gyrate",
                   value = as.double(gyrate$value))

}

