#' ED (landscape level)
#'
#' @description Edge Density (Area and Edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param count_boundary Count landscape boundary as edge
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{ED = \frac{E} {A} * 10000}
#' where \eqn{E} is the total landscape edge in meters and \eqn{A} is the total
#' landscape area in square meters.
#'
#' ED is an 'Area and Edge metric'. The edge density equals all edges in the landscape
#' in relation to the landscape area. The boundary of the landscape is only included in the
#' corresponding total class edge length if \code{count_boundary = TRUE}.
#' The metric describes the configuration of the landscape, e.g. because an overall aggregation
#' of  classes will result in a low edge density. The metric is standardized to the
#' total landscape area, and therefore comparisons among landscapes with different total
#' areas are possible.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Meters per hectare}
#' \subsection{Range}{ED >= 0}
#' \subsection{Behaviour}{Equals ED = 0 if only one patch is present (and the landscape
#' boundary is not included) and increases, without limit, as the landscapes becomes more
#' patchy}
#'
#' @seealso
#' \code{\link{lsm_l_te}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_c_ed}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_ed(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_l_ed <- function(landscape,
                          count_boundary = FALSE, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         ed <- lsm_l_ed_calc(
                             landscape_mat = landscape_mat,
                             count_boundary = count_boundary,
                             directions = directions,
                             resolution = resolution
                         )

                         lsm_landscape_output(metric = "ed", value = ed)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_ed_calc <- function(landscape_mat, count_boundary = FALSE, directions = 8, resolution = NULL,
                          classes = NULL, class_patches = NULL, area_patches = NULL, neighbor_matrix = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(as.double(NA))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(area_patches) || is.null(neighbor_matrix)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "area_patches", "neighbor_matrix"),
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        area_patches <- deps$area_patches
        neighbor_matrix <- deps$neighbor_matrix
    }

    # get patch area (handles lazy deps)
    area_patch <- lsm_p_area_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # summarise to total area
    area_total <- sum(area_patch)

    # get total edge
    edge_landscape <- lsm_l_te_calc(
        landscape_mat = landscape_mat,
        count_boundary = count_boundary,
        resolution = resolution,
        neighbor_matrix = neighbor_matrix
    )

    # relative edge density
    ed <- edge_landscape / area_total

    return(as.double(ed))
}
