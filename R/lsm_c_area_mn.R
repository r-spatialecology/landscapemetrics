#' AREA_MN (class level)
#'
#' @description Mean of patch area (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA_{MN} = mean(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares
#'
#' AREA_MN is an 'Area and Edge metric'. The metric summarises each class
#' as the mean of all patch areas belonging to class i. The metric is a simple way
#' to describe the composition of the landscape. Especially together with the total
#' class area (\code{\link{lsm_c_ca}}), it can also give an an idea of patch structure
#' (e.g. many small patches vs. few larges patches).
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_MN > 0}
#' \subsection{Behaviour}{Approaches AREA_MN = 0 if all patches are small. Increases, without
#' limit, as the patch areas increase.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link[base]{mean}}, \cr
#' \code{\link{lsm_c_area_cv}},
#' \code{\link{lsm_c_area_sd}}, \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_sd}},
#' \code{\link{lsm_l_area_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_area_mn(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_area_mn <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         area_mn <- lsm_c_area_mn_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "area_mn",
                                          class = as.integer(names(area_mn)),
                                          value = unname(area_mn))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_area_mn_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                                classes = NULL, class_patches = NULL, area_patches = NULL) {

    # reuse lsm_p_area_calc to get patch areas (handles lazy deps)
    area_patch <- lsm_p_area_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # all values NA
    if (all(is.na(unname(area_patch)))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # calculate mean by class using tapply on named vector
    area_mean <- tapply(area_patch, names(area_patch), mean)

    # return named vector
    stats::setNames(as.double(area_mean), names(area_mean))
}
