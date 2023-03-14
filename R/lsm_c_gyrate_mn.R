#' GYRATE_MN (class level)
#'
#' @description Mean radius of gyration (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param cell_center If true, the coordinates of the centroid are forced to be
#' a cell center within the patch.
#'
#' @details
#' \deqn{GYRATE_{MN} = mean(GYRATE[patch_{ij}])}
#' where \eqn{GYRATE[patch_{ij}]} equals the radius of gyration of each patch.
#'
#' GYRATE_MN is an 'Area and edge metric'. The metric summarises each class
#' as the mean of the radius of gyration of all patches belonging to class i.
#' GYRATE measures the distance from each cell to the patch centroid and is based on
#' cell center-to-cell center distances. The metrics characterises
#' both the patch area and compactness.
#'
#' If `cell_center = TRUE` some patches might have several possible cell-center
#' centroids. In this case, the gyrate index is based on the mean distance of all
#' cells to all possible cell-center centroids.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE_MN >= 0 }
#' \subsection{Behaviour}{Approaches GYRATE_MN = 0 if every patch is a single cell. Increases,
#' without limit, when only one patch is present.}
#'
#' @seealso
#' \code{\link{lsm_p_gyrate}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_gyrate_sd}},
#' \code{\link{lsm_c_gyrate_cv}}, \cr
#' \code{\link{lsm_l_gyrate_mn}},
#' \code{\link{lsm_l_gyrate_sd}},
#' \code{\link{lsm_l_gyrate_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_gyrate_mn(terra::unwrap(landscape))
#'
#' @aliases lsm_c_gyrate_mn
#' @rdname lsm_c_gyrate_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' Keitt, T. H., Urban, D. L., & Milne, B. T. 1997. Detecting critical scales
#' in fragmented landscapes. Conservation ecology, 1(1).
#'
#' @export
lsm_c_gyrate_mn <- function(landscape,
                                 directions = 8, cell_center = FALSE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_gyrate_mn_calc,
                     directions = directions,
                     cell_center = cell_center)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_gyrate_mn_calc <- function(landscape, directions, cell_center,
                                 points = NULL) {

    gyrate <- lsm_p_gyrate_calc(landscape,
                                directions = directions,
                                cell_center = cell_center,
                                points = points)

    # all cells are NA
    if (all(is.na(gyrate$value))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "gyrate_cv",
                              value = as.double(NA)))
    }

    gyrate_mn <-  stats::aggregate(x = gyrate[, 5], by = gyrate[, 2],
                                   FUN = mean)

    return(tibble::tibble(level = "class",
                          class = as.integer(gyrate_mn$class),
                          id = as.integer(NA),
                          metric = "gyrate_mn",
                          value = as.double(gyrate_mn$value)))
}
