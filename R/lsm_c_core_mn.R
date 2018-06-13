#' Core area distribution (class level)
#'
#' @description Mean of patch core area (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the mean of the patch core area of class i. The core area is
#' the area within a patch that is not on the edge of the patch of class i.
#' In other words, the area of a patch that has only neighbouring cells of the same type
#' of class i
#' \deqn{CORE_MN = mean(core[patch_i])}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
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
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_core_mn <- function(landscape) UseMethod("lsm_c_core_mn")

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_core_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_core_mn_calc <- function(landscape){

    core_mean <- landscape %>%
        lsm_p_core() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value))

    tibble::tibble(
        level = "class",
        class = core_mean$class,
        id = as.integer(NA),
        metric = "core area (mean)",
        value = core_mean$value
    )
}
