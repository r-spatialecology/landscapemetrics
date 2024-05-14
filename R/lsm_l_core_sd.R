#' CORE_SD (landscape level)
#'
#' @description Standard deviation of patch core area (class level)
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
#' \deqn{CORE_{SD} = sd(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_SD is a 'Core area metric'. It equals the standard deviation of the core area
#' of all patches in the landscape. The core area is defined as all cells that have no
#' neighbour with a different value than themselves (rook's case). The metric describes the
#' differences among all patches in the landscape.
#'
#' Because the metric is based on distances or areas please make sure your data
#' is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE_SD >= 0}
#' \subsection{Behaviour}{Equals CORE_SD = 0 if all patches have the same core area.
#' Increases, without limit, as the variation of patch core areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_sd}},
#' \code{\link{lsm_c_core_cv}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_cv}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_core_sd(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_l_core_sd <- function(landscape,
                               directions = 8,
                               consider_boundary = FALSE,
                               edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_core_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_core_sd_calc <- function(landscape, directions, consider_boundary, edge_depth, resolution, extras = NULL){

    core_patch <- lsm_p_core_calc(landscape,
                                  directions = directions,
                                  consider_boundary = consider_boundary,
                                  edge_depth = edge_depth,
                                  resolution = resolution,
                                  extras = extras)

    # all values NA
    if (all(is.na(core_patch$value))) {
        return(tibble::new_tibble(list(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "core_sd",
                              value = as.double(NA))))
    }

    core_sd <- stats::sd(core_patch$value)

    return(tibble::new_tibble(list(level = rep("landscape", length(core_sd)),
                 class = rep(as.integer(NA), length(core_sd)),
                 id = rep(as.integer(NA), length(core_sd)),
                 metric = rep("core_sd", length(core_sd)),
                 value = as.double(core_sd))))
}
