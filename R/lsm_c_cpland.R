#' CPLAND (class level)
#'
#' @description Core area percentage of landscape (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CPLAND = (\frac{\sum_{j = 1} ^ {n} a_{ij} ^ {core}}{A}) * 100}
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
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
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

    total_area <- lsm_l_ta_calc(landscape)

    cpland <- landscape %>%
        lsm_c_tca_calc() %>%
        dplyr::mutate(value = value / total_area$value * 100)

    tibble::tibble(
        level = "class",
        class = as.integer(cpland$class),
        id = as.integer(NA),
        metric = "core area percentage of landscape",
        value = as.double(cpland$value)
    )
}
