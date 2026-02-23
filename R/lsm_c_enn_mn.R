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
#' more isolated.
#' Also, this metric returns NA when the focal class contains only one patch}
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
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         enn_mn <- lsm_c_enn_mn_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             verbose = verbose,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "enn_mn",
                                          class = as.integer(names(enn_mn)),
                                          value = unname(enn_mn))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}


lsm_c_enn_mn_calc <- function(landscape_mat, directions, verbose, resolution, enn_patch = NULL) {

    # lazy dependency resolution
    if (is.null(enn_patch)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("enn_patch"),
            resolution = resolution
        )
        enn_patch <- deps$enn_patch
    }

    enn <- lsm_p_enn_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        verbose = verbose,
        resolution = resolution,
        enn_patch = enn_patch
    )

    # all cells are NA
    if (all(is.na(unname(enn)))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    enn_mn <- tapply(enn, names(enn), mean, na.rm = TRUE)

    # return named vector
    stats::setNames(as.double(enn_mn), names(enn_mn))
}
