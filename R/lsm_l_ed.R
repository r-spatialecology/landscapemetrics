#' Edge density (landscape level)
#'
#' @description Edge density (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param ... Specific arguments for certain functions, if not provided they fall back to default.
#'
#' @details
#' The edge density equals the sum of all edges in the landscape divided by the landscape area.
#' ED is scaled to 1 hectare and therefore compareable among landscapes with different total areas
#' \deqn{ED = total edge / total area}
#' \subsection{Units}{Meters per hectar}
#' \subsection{Range}{ED >= 0}
#' \subsection{Behaviour}{ED = 0 when only one class and patch is present and increases
#' without limit as edges increase, i.e. number of patches and classes}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ed(landscape)
#'
#' @aliases lsm_l_ed
#' @rdname lsm_l_ed
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_ed <- function(landscape, ...) UseMethod("lsm_l_ed")

#' @name lsm_l_ed
#' @export
lsm_l_ed.RasterLayer <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ed_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ed
#' @export
lsm_l_ed.RasterStack <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ed_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ed
#' @export
lsm_l_ed.RasterBrick <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ed_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ed
#' @export
lsm_l_ed.list <- function(landscape, ...) {
    purrr::map_dfr(landscape, lsm_l_ed_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_ed_calc <- function(landscape, ...) {

    area_landscape <- lsm_l_ta_calc(landscape)

    ed <- landscape %>%
        lsm_l_te_calc(...) %>%
        dplyr::mutate(value = value / area_landscape$value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "edge density",
        value = ed$value
    )
}
