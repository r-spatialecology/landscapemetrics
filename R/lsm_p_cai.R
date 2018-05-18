#' Core area index  (patch level)
#'
#' @description Core area divided by pathch area (patch level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_p_cai(landscape)
#' lsm_p_cai(landscape_stack)
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
    core_area <- lsm_p_core(landscape)
    area <- lsm_p_area(landscape)

    cai <- core_area$value / area$value * 100

        tibble::tibble(
            level = "patch",
            class = core_area$class,
            id = core_area$id,
            metric = "core area index",
            value = cai
    )
}


