#' SHAPE_CV (class level)
#'
#' @description Covariance of variation shape index (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SHAPE_{CV} = cv(SHAPE[patch_{ij}])}
#' where \eqn{SHAPE[patch_{ij}]} is the shape index of each patch.
#'
#' SHAPE_CV is a 'Shape metric'. Each class is summarised as the Coefficient of variation
#' of each patch belonging to class i. SHAPE describes the ratio between the actual perimeter of
#' the patch and the square root of patch area.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{SHAPE_CV >= 0}
#' \subsection{Behaviour}{Equals SHAPE_CV = 0 if all patches have an identical shape index.
#' Increases, without limit, as the variation of the shape index increases.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}}, \cr
#' \code{\link{lsm_c_shape_mn}},
#' \code{\link{lsm_c_shape_sd}}, \cr
#' \code{\link{lsm_l_shape_mn}},
#' \code{\link{lsm_l_shape_sd}},
#' \code{\link{lsm_l_shape_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_shape_cv(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Patton, D. R. 1975. A diversity index for quantifying habitat "edge".
#' Wildl. Soc.Bull. 3:171-173.
#'
#' @export
lsm_c_shape_cv <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         shape_cv <- lsm_c_shape_cv_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "shape_cv",
                                          class = as.integer(names(shape_cv)),
                                          value = unname(shape_cv))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_shape_cv_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                                 classes = NULL, class_patches = NULL, perimeter_patch = NULL, area_patches = NULL) {

    # reuse lsm_p_shape_calc to get shape values (handles lazy deps)
    shape_patch <- lsm_p_shape_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        perimeter_patch = perimeter_patch,
        area_patches = area_patches
    )

    # all cells are NA
    if (all(is.na(unname(shape_patch)))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # calculate cv by class using tapply on named vector
    shape_cv <- tapply(shape_patch, names(shape_patch), function(x) stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100)

    # return named vector
    stats::setNames(as.double(shape_cv), names(shape_cv))
}
