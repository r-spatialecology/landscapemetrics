#' CORE_CV (landscape level)
#'
#' @description Coefficient of variation of core area (Core area metric)
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
#' \deqn{CORE_{CV} = cv(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_CV is a 'Core area metric'. It equals the Coefficient of variation of the core area
#' of each patch in the landscape. The core area is defined as all cells that have no
#' neighbour with a different value than themselves (rook's case). The metric describes the
#' differences among all patches in the landscape and is easily comparable
#' because it is scaled to the mean.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE_CV >= 0}
#' \subsection{Behaviour}{Equals CORE_CV = 0 if all patches have the same core area.
#' Increases, without limit, as the variation of patch core areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_core}}, \cr
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_sd}},
#' \code{\link{lsm_c_core_cv}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_sd}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_core_cv(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_l_core_cv <- function(landscape,
                               directions = 8,
                               consider_boundary = FALSE,
                               edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         core_cv <- lsm_l_core_cv_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             consider_boundary = consider_boundary,
                             edge_depth = edge_depth,
                             resolution = resolution
                         )

                         lsm_landscape_output(metric = "core_cv", value = core_cv)
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_core_cv_calc <- function(landscape_mat, directions = NULL, consider_boundary = FALSE, edge_depth = 1, resolution = NULL,
                               classes = NULL, class_patches = NULL, core_patch = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(as.double(NA))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches) || is.null(core_patch)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches", "core_patch"),
            consider_boundary = consider_boundary,
            edge_depth = edge_depth,
            resolution = resolution
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
        core_patch <- deps$core_patch
    }

    # all values NA
    if (all(is.na(core_patch))) {
        return(as.double(NA))
    }

    core_cv <- stats::sd(core_patch) / mean(core_patch) * 100

    return(as.double(core_cv))
}
