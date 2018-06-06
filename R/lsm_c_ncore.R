#' Number of core areas (class level)
#'
#' @description Called number of disjunct core areas in FRAGSTATS (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ncore(landscape)
#'
#' @aliases lsm_c_ncore
#' @rdname lsm_c_ncore
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_ncore <- function(landscape) UseMethod("lsm_c_ncore")

#' @name lsm_c_ncore
#' @export
lsm_c_ncore.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ncore
#' @export
lsm_c_ncore.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ncore
#' @export
lsm_c_ncore.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ncore
#' @export
lsm_c_ncore.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_ncore_calc <- function(landscape){
    ncore <- landscape %>%
        lsm_p_ncore() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value))

    tibble::tibble(
        level = "class",
        class = ncore$class,
        id = as.integer(NA),
        metric = "number of core areas",
        value = ncore$value
    )
}
