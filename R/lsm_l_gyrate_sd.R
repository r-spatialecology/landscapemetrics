#' GYRATE_SD (landscape level)
#'
#' @description Standard deviation radius of gyration (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param cell_center If true, the coordinates of the centroid are forced to be
#' a cell center within the patch.
#'
#' @details
#' \deqn{GYRATE_{SD} = sd(GYRATE[patch_{ij}])}
#' where \eqn{GYRATE[patch_{ij}]} equals the radius of gyration of each patch.
#'
#' GYRATE_SD is an 'Area and edge metric'. The metric summarises the landscape
#' as the standard deviation of the radius of gyration of all patches
#' in the landscape. GYRATE measures the distance from each cell to the patch
#' centroid and is based on cell center-to-cell center distances. The metrics characterises
#' both the patch area and compactness.
#'
#' If `cell_center = TRUE` some patches might have several possible cell-center
#' centroids. In this case, the gyrate index is based on the mean distance of all
#' cells to all possible cell-center centroids.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE_SD >= 0 }
#' \subsection{Behaviour}{Equals GYRATE_SD = 0 if the radius of gyration is identical
#' for all patches. Increases, without limit, as the variation of the radius of gyration
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_gyrate}}, \cr
#' \code{\link{lsm_c_gyrate_mn}},
#' \code{\link{lsm_c_gyrate_sd}},
#' \code{\link{lsm_c_gyrate_cv}}, \cr
#' \code{\link{lsm_l_gyrate_mn}},
#' \code{\link{lsm_l_gyrate_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_gyrate_sd(landscape)
#'
#' @aliases lsm_l_gyrate_sd
#' @rdname lsm_l_gyrate_sd
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
lsm_l_gyrate_sd <- function(landscape,
                                 directions = 8, cell_center = FALSE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_gyrate_sd_calc,
                     directions = directions,
                     cell_center = cell_center)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_gyrate_sd_calc <- function(landscape, directions, cell_center, extras = NULL) {

    gyrate_patch <- lsm_p_gyrate_calc(landscape,
                                      directions = directions,
                                      cell_center = cell_center,
                                      extras = extras)

    # all values NA
    if (all(is.na(gyrate_patch$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "gyrate_sd",
                              value = as.double(NA)))
    }

    gyrate_sd <- stats::sd(gyrate_patch$value)

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "gyrate_sd",
                          value = as.double(gyrate_sd)))
}

