#' CPLAND (class level)
#'
#' @description Core area percentage of landscape (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 <= CPLAND < 100}
#' \subsection{Behaviour}{Approaches CPLAND = 0 if CORE = 0 for all patches. Increases as
#' the amount of core area increases, i.e. patches become larger while beiing rather simple
#' in shape.}
#'
#' @seealso \code{\link{lsm_p_core}} and \code{\link{lsm_l_ta}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cpland(landscape)
#'
#' @aliases lsm_c_cpland
#' @rdname lsm_c_cpland
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_cpland <- function(landscape) UseMethod("lsm_c_cpland")

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cpland_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cpland_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cpland_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cpland
#' @export
lsm_c_cpland.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_cpland_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_cpland_calc <- function(landscape){

    area_landscape <- lsm_l_ta_calc(landscape)

    core_area_class <- lsm_c_tca_calc(landscape)

    cpland <- dplyr::mutate(core_area_class, value = value / area_landscape$value * 100)

    tibble::tibble(
        level = "class",
        class = as.integer(cpland$class),
        id = as.integer(NA),
        metric = "cpland",
        value = as.double(cpland$value)
    )
}
