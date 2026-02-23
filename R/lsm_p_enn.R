#' ENN (patch level)
#'
#' @description Euclidean Nearest-Neighbor Distance (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @details
#' \deqn{ENN = h_{ij}}
#' where \eqn{h_{ij}} is the distance to the nearest neighbouring patch of
#' the same class i in meters
#'
#' ENN is an 'Aggregation metric'. The distance to the nearest neighbouring patch of
#' the same class i. The distance is measured from edge-to-edge. The range is limited by the
#' cell resolution on the lower limit and the landscape extent on the upper limit. The metric
#' is a simple way to describe patch isolation.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{ENN > 0}
#' \subsection{Behaviour}{Approaches ENN = 0 as the distance to the nearest neighbour
#' decreases, i.e. patches of the same class i are more aggregated. Increases, without limit,
#' as the distance between neighbouring patches of the same class i increases, i.e. patches are
#' more isolated.}
#'
#' @seealso
#' \code{\link{lsm_c_enn_mn}},
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
#' lsm_p_enn(landscape)
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
lsm_p_enn <- function(landscape, directions = 8, verbose = TRUE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         enn <- lsm_p_enn_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             verbose = verbose,
                             resolution = resolution
                         )

                         lsm_patch_output(metric = "enn",
                                          class = as.integer(names(enn)),
                                          value = unname(enn),
                                          id = seq_along(enn))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_enn_calc <- function(landscape_mat, directions, verbose, resolution, enn_patch = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

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

    # enn_patch is a named vector
    # names are class IDs, values are ENN distances
    enn_vec <- enn_patch

    # return named vector (preserve names)
    structure(as.double(enn_vec), names = names(enn_vec))
}
