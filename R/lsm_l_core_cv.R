#' CORE_CV (landscape level)
#'
#' @description Coeffiecent of variation of core area (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CORE_{CV} = cv(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_CV is a 'Core area metric'. It equals the coeffiecent of variation of the core area
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
#' \code{\link{lsm_p_core}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_sd}},
#' \code{\link{lsm_c_core_cv}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_sd}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_core_cv(landscape)
#'
#' @aliases lsm_l_core_cv
#' @rdname lsm_l_core_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_core_cv <- function(landscape, directions) UseMethod("lsm_l_core_cv")

#' @name lsm_l_core_cv
#' @export
lsm_l_core_cv.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_core_cv_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_core_cv
#' @export
lsm_l_core_cv.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_core_cv_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_core_cv
#' @export
lsm_l_core_cv.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_core_cv_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_core_cv
#' @export
lsm_l_core_cv.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_core_cv_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_core_cv_calc <- function(landscape, directions){

    core_cv <- landscape %>%
        lsm_p_core_calc(., directions = directions) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "core_cv",
        value = as.double(core_cv$value)
    )
}
