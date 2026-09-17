#' COHESION (class level)
#'
#' @description Patch Cohesion Index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{COHESION = 1 - (\frac{\sum \limits_{j = 1}^{n} p_{ij}} {\sum \limits_{j = 1}^{n} p_{ij} \sqrt{a_{ij}}}) * (1 - \frac{1} {\sqrt{Z}}) ^ {-1} * 100}
#' where \eqn{p_{ij}} is the perimeter in meters, \eqn{a_{ij}} is the area in square
#' meters and \eqn{Z} is the number of cells.
#'
#' COHESION is an 'Aggregation metric'. It characterises the connectedness of patches
#' belonging to class i. It can be used to asses if patches of the same class are located
#' aggregated or rather isolated and thereby COHESION gives information about the
#' configuration of the landscape.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Percent}
#' \subsection{Ranges}{0 < COHESION < 100}
#' \subsection{Behaviour}{Approaches COHESION = 0 if patches of class i become more isolated.
#' Increases if patches of class i become more aggregated.}
#'
#' @seealso
#' \code{\link{lsm_p_perim}},
#' \code{\link{lsm_p_area}}, \cr
#' \code{\link{lsm_l_cohesion}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_cohesion(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' Schumaker, N. H. 1996. Using landscape indices to predict habitat
#' connectivity. Ecology, 77(4), 1210-1225.
#'
#' @export
lsm_c_cohesion <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         cohesion <- lsm_c_cohesion_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "cohesion",
                                          class = as.integer(names(cohesion)),
                                          value = unname(cohesion))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_cohesion_calc <- function(landscape_mat, directions = NULL, resolution = NULL,
                                classes = NULL, class_patches = NULL, area_patches = NULL, perimeter_patch = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(area_patches) || is.null(perimeter_patch)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "area_patches", "perimeter_patch"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        area_patches <- deps$area_patches
        perimeter_patch <- deps$perimeter_patch
    }

    # get number of cells (only not NAs)
    ncells_landscape <- sum(!is.na(landscape_mat))

    # get patch area (handles lazy deps)
    patch_area <- lsm_p_area_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # get number of cells for each patch -> area = n_cells * res / 10000
    ncells_patch <- patch_area * 10000 / prod(resolution)

    # get perim of patch
    perim_patch <- lsm_p_perim_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        perimeter_patch = perimeter_patch
    )

    # calculate denominator of cohesion
    perim_x_sqrt_ncells <- perim_patch * sqrt(ncells_patch)

    # group by class and sum using tapply on named vectors
    denominator <- tapply(perim_x_sqrt_ncells, names(perim_x_sqrt_ncells), sum, na.rm = TRUE)
    cohesion_sum <- tapply(perim_patch, names(perim_patch), sum, na.rm = TRUE)

    # calculate cohesion
    cohesion <- (1 - (cohesion_sum / denominator)) *
        ((1 - (1 / sqrt(ncells_landscape))) ^ -1) * 100

    # return named vector
    stats::setNames(as.double(cohesion), names(cohesion))
}
