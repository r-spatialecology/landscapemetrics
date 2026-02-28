#' CAI (patch level)
#'
#' @description Core area index (Core area metric)
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
#' \deqn{CAI = (\frac{a_{ij}^{core}} {a_{ij}}) * 100}
#' where \eqn{a_{ij}^{core}} is the core area in square meters and \eqn{a_{ij}}
#' is the area in square meters.
#'
#' CAI is a 'Core area metric'. It equals the percentage of a patch that is core area.
#' A cell is defined as core area if the cell has no neighbour with a different value
#' than itself (rook's case). It describes patch area and shape simultaneously (more core area
#' when the patch is large and the shape is rather compact, i.e. a square). Because the index is
#' relative, it is comparable among patches with different area.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{0 <= CAI <= 100}
#' \subsection{Behaviour}{CAI = 0 when the patch has no core area and
#' approaches CAI = 100 with increasing percentage of core area within a patch.}
#'
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link{lsm_p_area}}, \cr
#' \code{\link{lsm_c_cai_mn}},
#' \code{\link{lsm_c_cai_sd}},
#' \code{\link{lsm_c_cai_cv}},
#' \code{\link{lsm_c_cpland}}, \cr
#' \code{\link{lsm_l_cai_mn}},
#' \code{\link{lsm_l_cai_sd}},
#' \code{\link{lsm_l_cai_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_p_cai(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_p_cai <- function(landscape,
                      directions = 8,
                      consider_boundary = FALSE,
                      edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         cai <- lsm_p_cai_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             consider_boundary = consider_boundary,
                             edge_depth = edge_depth,
                             resolution = resolution
                         )

                         lsm_patch_output(metric = "cai",
                                          class = as.integer(names(cai)),
                                          value = unname(cai),
                                          id = seq_along(cai))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_cai_calc <- function(landscape_mat, directions, consider_boundary, edge_depth, resolution,
                           classes = NULL, class_patches = NULL, area_patches = NULL, core_patch = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(area_patches) || is.null(core_patch)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "area_patches", "core_patch"),
            consider_boundary = consider_boundary,
            edge_depth = edge_depth,
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        area_patches <- deps$area_patches
        core_patch <- deps$core_patch
    }

    # get patch area
    area_patch <- lsm_p_area_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches,
        area_patches = area_patches
    )

    # calculate CAI index
    cai_patch <- core_patch / area_patch * 100

    # return named vector (preserve names)
    structure(as.double(cai_patch), names = names(cai_patch))
}
