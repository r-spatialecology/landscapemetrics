#' ENN_MN (class level)
#'
#' @description Mean of euclidean nearest-neighbor distance (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @details
#' \deqn{ENN_{MN} = mean(ENN[patch_{ij}])}
#' where \eqn{ENN[patch_{ij}]} is the euclidean nearest-neighbor distance
#' of each patch.
#'
#' ENN_MN is an 'Aggregation metric'. It summarises each class as the mean of each patch
#' belonging to class i. ENN measures the distance to the  nearest neighbouring patch
#' of the same class i. The distance is measured from edge-to-edge. The range is limited
#' by the cell resolution on the lower limit and the landscape extent on the upper limit.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{ENN_MN > 0}
#' \subsection{Behaviour}{Approaches ENN_MN = 0 as the distance to the nearest neighbour
#' decreases, i.e. patches of the same class i are more aggregated. Increases, without limit,
#' as the distance between neighbouring patches of the same class i increases, i.e. patches are
#' more isolated.}
#'
#' @seealso
#' \code{\link{lsm_p_enn}},
#' \code{\link[base]{mean}}, \cr
#' \code{\link{lsm_c_enn_sd}},
#' \code{\link{lsm_c_enn_cv}}, \cr
#' \code{\link{lsm_l_enn_mn}},
#' \code{\link{lsm_l_enn_sd}},
#' \code{\link{lsm_l_enn_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_enn_mn(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' McGarigal, K., and McComb, W. C. (1995). Relationships between landscape
#' structure and breeding birds in the Oregon Coast Range.
#' Ecological monographs, 65(3), 235-260.
#'
#' @export
lsm_c_enn_mn <- function(landscape, directions = 8, verbose = TRUE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_enn_mn_calc,
                     directions = directions,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}


lsm_c_enn_mn_calc <- function(landscape, directions, verbose, resolution, extras = NULL) {

    enn <- lsm_p_enn_calc(landscape,
                          directions = directions,
                          verbose = verbose,
                          resolution = resolution, extras = extras)

    # all cells are NA
    if (all(is.na(enn$value))) {
        return(tibble::new_tibble(list(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "enn_mn",
                              value = as.double(NA))))
    }

    enn_mn <- stats::aggregate(x = enn[, 5], by = enn[, 2], FUN = mean)

    return(tibble::new_tibble(list(
        level = rep("class", nrow(enn_mn)),
        class = as.integer(enn_mn$class),
        id = rep(as.integer(NA), nrow(enn_mn)),
        metric = rep("enn_mn", nrow(enn_mn)),
        value = as.double(enn_mn$value)
    )))
}
