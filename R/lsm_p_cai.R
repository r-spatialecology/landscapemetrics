#' Core area index  (patch level)
#'
#' @description Core area index of patch (patch level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The core area index equals the percentage of a patch that is core area
#' \deqn{CAI = (core[patch] / area[patch]) * 100}
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 <= CAI <= 100}
#' \subsection{Behaviour}{CAI = 0 when the patch has no core area and
#' approaches CAI = 100 with increasing percentage of core area within a patch}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_cai(landscape)
#'
#' @aliases lsm_p_cai
#' @rdname lsm_p_cai
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_cai <- function(landscape) UseMethod("lsm_p_cai")

#' @name lsm_p_cai
#' @export
lsm_p_cai.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_cai_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_cai
#' @export
lsm_p_cai.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_cai_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_cai
#' @export
lsm_p_cai.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_cai_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_cai
#' @export
lsm_p_cai.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_cai_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_cai_calc <- function(landscape){

    area <- lsm_p_area(landscape) %>%
        dplyr::mutate(value = value * 10000)

    cai <- landscape %>%
        lsm_p_core() %>%
        dplyr::mutate(value = value * 10000 / area$value * 100)

    tibble::tibble(
        level = "patch",
        class = cai$class,
        id = cai$id,
        metric = "core area index",
        value = cai$value
    )
}
