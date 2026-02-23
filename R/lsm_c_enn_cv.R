#' ENN_CV (class level)
#'
#' @description Coefficient of variation of euclidean nearest-neighbor distance (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @details
#' \deqn{ENN_{CV} = cv(ENN[patch_{ij}])}
#' where \eqn{ENN[patch_{ij}]} is the euclidean nearest-neighbor distance
#' of each patch.
#'
#' ENN_CV is an 'Aggregation metric'. It summarises each class as the Coefficient
#' of variation of each patch belonging to class i. ENN measures the distance to the  nearest
#' neighbouring patch of the same class i. The distance is measured from edge-to-edge.
#' The range is limited by the cell resolution on the lower limit and the landscape extent
#' on the upper limit. The metric is a simple way to describe patch isolation. Because it is
#' scaled to the mean, it is easily comparable among different landscapes.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{ENN_CV >= 0}
#' \subsection{Behaviour}{Equals ENN_CV = 0 if the euclidean nearest-neighbor distance is
#' identical for all patches. Increases, without limit, as the variation of ENN increases.
#' Also, this metric returns NA when the focal class contains only one patch}
#'
#' @seealso
#' \code{\link{lsm_p_enn}}, \cr
#' \code{\link{lsm_c_enn_mn}},
#' \code{\link{lsm_c_enn_sd}}, \cr
#' \code{\link{lsm_l_enn_mn}},
#' \code{\link{lsm_l_enn_sd}},
#' \code{\link{lsm_l_enn_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_enn_cv(landscape)
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
lsm_c_enn_cv <- function(landscape, directions = 8, verbose = TRUE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         enn_cv <- lsm_c_enn_cv_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             verbose = verbose,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "enn_cv",
                                          class = as.integer(names(enn_cv)),
                                          value = unname(enn_cv))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_enn_cv_calc <- function(landscape_mat, directions, verbose, resolution) {

    deps <- resolve_extras(
        landscape_mat = landscape_mat,
        directions = directions,
        required = c("enn_patch"),
        resolution = resolution
    )
    enn_patch <- deps$enn_patch

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

    enn_cv <- tapply(enn, names(enn), function(x) stats::sd(x) / mean(x) * 100)

    # return named vector
    stats::setNames(as.double(enn_cv), names(enn_cv))
}
