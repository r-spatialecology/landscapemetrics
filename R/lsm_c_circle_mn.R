#' CIRCLE_MN (Class level)
#'
#' @description Mean of related circumscribing circle (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CIRCLE_{MN} = mean(CIRCLE[patch_{ij}])}
#' where \eqn{CIRCLE[patch_{ij}]} is the related circumscribing circle of each patch.
#'
#' CIRCLE_MN is a 'Shape metric' and summarises each class as the mean of the related
#' circumscribing circle of all patches belonging to class i. CIRCLE describes
#' the ratio between the patch area and the smallest circumscribing circle of the patch
#' and characterises the compactness of the patch.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CIRCLE_MN > 0}
#' \subsection{Behaviour}{Approaches CIRCLE_MN = 0 if the related circumscribing circle
#' of all patches is small. Increases, without limit, as the related circumscribing circles
#' increase.}
#'
#' @seealso
#' \code{\link{lsm_p_circle}},
#' \code{\link[base]{mean}}, \cr
#' \code{\link{lsm_c_circle_sd}},
#' \code{\link{lsm_c_circle_cv}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_sd}},
#' \code{\link{lsm_l_circle_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_circle_mn(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Baker, W. L., and Y. Cai. 1992. The r.le programs for multiscale analysis of
#' landscape structure using the GRASS geographical information system.
#' Landscape Ecology 7: 291-302.
#'
#' Based on C++ code from Project Nayuki (https://www.nayuki.io/page/smallest-enclosing-circle).
#'
#' @export
lsm_c_circle_mn <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         circle_mn <- lsm_c_circle_mn_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "circle_mn",
                                          class = as.integer(names(circle_mn)),
                                          value = unname(circle_mn))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_circle_mn_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                                 classes = NULL, class_patches = NULL, area_patches = NULL) {

    # calculate circumscribing circle for each patch (handles lazy deps)
    circle <- lsm_p_circle_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # all values NA
    if (all(is.na(unname(circle)))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # summarise for classes using tapply on named vector
    circle_mn <- tapply(circle, names(circle), mean)

    # return named vector
    stats::setNames(as.double(circle_mn), names(circle_mn))
}
