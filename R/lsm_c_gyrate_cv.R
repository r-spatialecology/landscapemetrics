#' GYRATE_CV (class level)
#'
#' @description Coefficient of variation radius of gyration (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param cell_center If true, the coordinates of the centroid are forced to be
#' a cell center within the patch.
#'
#' @details
#' \deqn{GYRATE_{CV} = cv(GYRATE[patch_{ij}])}
#' where \eqn{GYRATE[patch_{ij}]} equals the radius of gyration of each patch.
#'
#' GYRATE_CV is an 'Area and edge metric'. The metric summarises each class
#' as the Coefficient of variation of the radius of gyration of all patches
#' belonging to class i. GYRATE measures the distance from each cell to the patch
#' centroid and is based on cell center-to-cell center distances. The metrics characterises
#' both the patch area and compactness. The Coefficient of variation is
#' scaled to the mean and comparable among different landscapes. Because the metric
#' is based on distances or areas please make sure your data is valid using
#' \code{\link{check_landscape}}.
#'
#' If `cell_center = TRUE` some patches might have several possible cell-center
#' centroids. In this case, the gyrate index is based on the mean distance of all
#' cells to all possible cell-center centroids.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE_CV >= 0 }
#' \subsection{Behaviour}{Equals GYRATE_CV = 0 if the radius of gyration is identical
#' for all patches. Increases, without limit, as the variation of the radius of gyration
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_gyrate}}, \cr
#' \code{\link{lsm_c_gyrate_mn}},
#' \code{\link{lsm_c_gyrate_sd}}, \cr
#' \code{\link{lsm_l_gyrate_mn}},
#' \code{\link{lsm_l_gyrate_sd}},
#' \code{\link{lsm_l_gyrate_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_gyrate_cv(landscape)
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
lsm_c_gyrate_cv <- function(landscape,
                                 directions = 8, cell_center = FALSE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_gyrate_cv_calc,
                     directions = directions,
                     cell_center = cell_center)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_gyrate_cv_calc <- function(landscape, directions, cell_center, resolution, extras = NULL) {

    gyrate <- lsm_p_gyrate_calc(landscape,
                                directions = directions,
                                cell_center = cell_center,
                                resolution = resolution,
                                extras = extras)

    # all cells are NA
    if (all(is.na(gyrate$value))) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "gyrate_cv",
                              value = as.double(NA))))
    }

    gyrate_cv <- stats::aggregate(x = gyrate[, 5], by = gyrate[, 2],
                                  FUN = function(x) stats::sd(x) / mean(x) * 100)

    return(tibble::new_tibble(list(
        level = rep("class", nrow(gyrate_cv)),
        class = as.integer(gyrate_cv$class),
        id = rep(as.integer(NA), nrow(gyrate_cv)),
        metric = rep("gyrate_cv", nrow(gyrate_cv)),
        value = as.double(gyrate_cv$value)
    )))
}
