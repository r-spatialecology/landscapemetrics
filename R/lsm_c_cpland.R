#' CPLAND (class level)
#'
#' @description Core area percentage of landscape (Core area metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell
#'
#' @details
#' \deqn{CPLAND = (\frac{\sum \limits_{j = 1}^{n} a_{ij}^{core}} {A}) * 100}
#' where \eqn{a_{ij}^{core}} is the core area in square meters and \eqn{A}
#' is the total landscape area in square meters.
#'
#' CPLAND is a 'Core area metric'. It is the percentage of core area of class i in relation to
#' the total landscape area. A cell is defined as core area if the cell has
#' no neighbour with a different value than itself (rook's case). Because CPLAND is
#' a relative measure, it is comparable among landscapes with different total areas.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 <= CPLAND < 100}
#' \subsection{Behaviour}{Approaches CPLAND = 0 if CORE = 0 for all patches. Increases as
#' the amount of core area increases, i.e. patches become larger while being rather simple
#' in shape.}
#'
#' @seealso \code{\link{lsm_p_core}} and \code{\link{lsm_l_ta}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_cpland(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_cpland <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         cpland <- lsm_c_cpland_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             consider_boundary = consider_boundary,
                             edge_depth = edge_depth,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "cpland",
                                          class = as.integer(names(cpland)),
                                          value = unname(cpland))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_cpland_calc <- function(landscape_mat, directions, consider_boundary, edge_depth, resolution,
                              classes = NULL, class_patches = NULL, area_patches = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

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

    # calculate patch area (in ha) (handles lazy deps)
    area <- lsm_p_area_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # total landscape area (in ha)
    total_area_ha <- sum(unname(area))

    # get core area for each patch (in ha) (returns named vector)
    core_area <- lsm_p_core_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        consider_boundary = consider_boundary,
        edge_depth = edge_depth,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches
    )

    # summarise core area for classes using tapply on named vector
    core_area_by_class <- tapply(core_area, names(core_area), sum, na.rm = TRUE)

    # relative core area of each class (both in ha, so no conversion needed)
    cpland <- core_area_by_class / total_area_ha * 100

    # return named vector
    stats::setNames(as.double(cpland), names(cpland))
}
