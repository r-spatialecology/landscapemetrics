#' CORE_CV (landscape level)
#'
#' @description Coefficient of variation of core area (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
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
#' lsm_l_core_cv(terra::unwrap(landscape))
#'
#' @aliases lsm_l_core_cv
#' @rdname lsm_l_core_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' @export
lsm_l_core_cv <- function(landscape,
                               directions = 8,
                               consider_boundary = FALSE,
                               edge_depth = 1) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_core_cv_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_core_cv_calc <- function(landscape, directions, consider_boundary, edge_depth, resolution = NULL){

    core_patch <- lsm_p_core_calc(landscape,
                                  directions = directions,
                                  consider_boundary = consider_boundary,
                                  edge_depth = edge_depth,
                                  resolution = resolution)

    # all values NA
    if (all(is.na(core_patch$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "core_cv",
                              value = as.double(NA)))
    }

    core_cv <- sd(core_patch$value) / mean(core_patch$value) * 100

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "core_cv",
                          value = as.double(core_cv)))
}
