#' CORE_MN (class level)
#'
#' @description Mean of core area (Core area metric)
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell
#'
#' @details
#' \deqn{CORE_{MN} = mean(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_MN is a 'Core area metric' and equals the mean of core areas of all patches
#' belonging to class i. The core area is defined as all cells that have no
#' neighbour with a different value than themselves (rook's case).
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE_MN >= 0}
#' \subsection{Behaviour}{Equals CORE_MN = 0 if CORE = 0 for all patches. Increases,
#' without limit, as the core area indices increase.}
#'
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link[base]{mean}}, \cr
#' \code{\link{lsm_c_core_sd}},
#' \code{\link{lsm_c_core_cv}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_sd}},
#' \code{\link{lsm_l_core_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_core_mn(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_core_mn <- function(landscape, directions = 8, consider_boundary = FALSE, edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = function(x) {
                         resolution <- terra::res(x)
                         landscape_mat <- terra::as.matrix(x, wide = TRUE)

                         core_mn <- lsm_c_core_mn_calc(
                             landscape_mat = landscape_mat,
                             directions = directions,
                             consider_boundary = consider_boundary,
                             edge_depth = edge_depth,
                             resolution = resolution
                         )

                         lsm_class_output(metric = "core_mn",
                                          class = as.integer(names(core_mn)),
                                          value = unname(core_mn))
                     })

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_core_mn_calc <- function(landscape_mat, directions = NULL, consider_boundary = FALSE, edge_depth = 1, resolution = NULL,
                               classes = NULL, class_patches = NULL) {

    # all values NA
    if (all(is.na(landscape_mat))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # lazy dependency resolution
    if (is.null(classes) || is.null(class_patches)) {
        deps <- resolve_extras(
            landscape_mat = landscape_mat,
            directions = directions,
            required = c("classes", "class_patches")
        )
        classes <- deps$classes
        class_patches <- deps$class_patches
    }

    core_patch <- lsm_p_core_calc(
        landscape_mat = landscape_mat,
        directions = directions,
        consider_boundary = consider_boundary,
        edge_depth = edge_depth,
        resolution = resolution,
        classes = classes,
        class_patches = class_patches
    )

    # all values NA
    if (all(is.na(unname(core_patch)))) {
        return(stats::setNames(as.double(NA), NA_character_))
    }

    # summarise for class using tapply on named vector
    core_mean <- tapply(core_patch, names(core_patch), mean)

    # return named vector
    stats::setNames(as.double(core_mean), names(core_mean))
}
