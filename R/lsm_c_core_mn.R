#' CORE_MN (class level)
#'
#' @description Mean of core area (Core area metric)
#' @param directions The number of directions in which cells should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.

#' @details
#' \deqn{CORE_{MN} = mean(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_MN is a 'Core area metric' and equals the mean of core areas of all patches
#' belonging to class i. The core area is defined as all cells that have no
#' neighbour with a different value than themselves (rook's case).
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE_MN >= 0}
#' \subsection{Behaviour}{Equals CORE_MN = 0 if CORE = 0 for all patches. Increases,
#' without limit, as the corea area indices increase.}
#'
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_core_sd}},
#' \code{\link{lsm_c_core_cv}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_sd}},
#' \code{\link{lsm_l_core_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_core_mn(landscape)
#'
#' @aliases lsm_c_core_mn
#' @rdname lsm_c_core_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_core_mn <- function(landscape, directions) UseMethod("lsm_c_core_mn")

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_mn_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_mn_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_mn_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_c_core_mn_calc, directions = directions,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_core_mn_calc <- function(landscape, directions){

    core_mean <- landscape %>%
        lsm_p_core_calc(., directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(core_mean$class),
        id = as.integer(NA),
        metric = "core_mn",
        value = as.double(core_mean$value)
    )
}
