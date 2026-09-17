#' CA (class level)
#'
#' @description Total (class) area (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CA = sum(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares.
#'
#' CA is an 'Area and edge metric' and a measure of composition.
#' The total (class) area sums the area of all patches belonging to class i.
#' It shows if the landscape is e.g. dominated by one class or if all classes
#' are equally present. CA is an absolute measure, making comparisons among
#' landscapes with different total areas difficult.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CA > 0}
#' \subsection{Behaviour}{Approaches CA > 0 as the patch areas of class i
#' become small. Increases, without limit, as the patch areas of class i become
#' large. CA = TA if only one class is present.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{sum}}, \cr
#' \code{\link{lsm_l_ta}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_ca(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_ca <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         ca <- lsm_c_ca_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "ca",
                                          class = as.integer(names(ca)),
                                          value = unname(ca))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_ca_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
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

    # summarise for each class using tapply on named vector
    ca <- tapply(area_patch, names(area_patch), sum, na.rm = TRUE)

    # return named vector
    stats::setNames(as.double(ca), names(ca))
}
