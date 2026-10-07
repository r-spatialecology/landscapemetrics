#' LPI (class level)
#'
#' @description Largest patch index (Area and Edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{LPI = \frac{\max \limits_{j = 1}^{n} (a_{ij})} {A} * 100}
#' where \eqn{max(a_{ij})} is the area of the patch in square meters and \eqn{A}
#' is the total landscape area in square meters.
#'
#' The largest patch index is an 'Area and edge metric'. It is the percentage of the
#' landscape covered by the corresponding largest patch of each class i. It is a simple
#' measure of dominance.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 < LPI <= 100}
#' \subsection{Behaviour}{Approaches LPI = 0 when the largest patch is becoming small
#' and equals LPI = 100 when only one patch is present}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_lpi}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_lpi(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_lpi <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         lpi <- lsm_c_lpi_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "lpi",
                                          class = as.integer(names(lpi)),
                                          value = unname(lpi))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_lpi_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
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

    # get patch area (handles lazy deps)
    patch_area <- lsm_p_area_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # all cells are NA
    if (all(is.na(unname(patch_area)))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # summarise to total area
    total_area <- sum(unname(patch_area))

    # calculate largest patch index
    patch_area <- patch_area / total_area * 100

    # summarise for each class using tapply on named vector
    lpi <- tapply(patch_area, names(patch_area), max, na.rm = TRUE)

    # return named vector
    stats::setNames(as.double(lpi), names(lpi))
}
