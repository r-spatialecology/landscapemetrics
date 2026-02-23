#' PLAND (class level)
#'
#' @description Percentage of landscape of class (Area and Edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PLAND = \frac{\sum \limits_{j = 1}^{n} a_{ij}} {A} * 100}
#' where \eqn{a_{ij}} is the area of each patch and \eqn{A} is the total
#' landscape area.
#'
#' PLAND is an 'Area and edge metric'. It is the percentage of the landscape
#' belonging to class i. It is a measure of composition and because of the relative
#' character directly comparable among landscapes with different total areas.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 < PLAND <= 100}
#' \subsection{Behaviour}{Approaches PLAND = 0 when the proportional class area is decreasing.
#' Equals PLAND = 100 when only one patch is present.}
#'
#' @seealso
#' \code{\link{lsm_c_ca}},
#' \code{\link{lsm_l_ta}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_pland(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_pland <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         pland <- lsm_c_pland_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "pland",
                                          class = as.integer(names(pland)),
                                          value = unname(pland))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_pland_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                             classes = NULL, class_patches = NULL, area_patches = NULL) {

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(area_patches)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "area_patches"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        area_patches <- deps$area_patches
    }

    pland <- lsm_p_area_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # all values NA
    if (all(is.na(unname(pland)))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # aggregate by class using tapply on named vector
    pland_sum <- tapply(pland, names(pland), sum, na.rm = TRUE)
    pland_sum <- pland_sum / sum(pland_sum) * 100

    # return named vector
    stats::setNames(as.double(pland_sum), names(pland_sum))
}
