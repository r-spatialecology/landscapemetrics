#' Core area distribution (landscape level)
#'
#' @description Mean of patch core area (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the mean of the patch core area of all patches in the landscape.
#' The core area is the area within a patch that is not on the edge of the patch of class i.
#' In other words, the area of a patch that has only neighbouring cells of the same type
#' of class i
#' \deqn{CORE_MN = mean(core[patch])}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_core_mn(landscape)
#'
#' @aliases lsm_l_core_mn
#' @rdname lsm_l_core_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_core_mn <- function(landscape, directions) UseMethod("lsm_l_core_mn")

#' @name lsm_l_core_mn
#' @export
lsm_l_core_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_core_mn_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_core_mn
#' @export
lsm_l_core_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_core_mn_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_core_mn
#' @export
lsm_l_core_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_core_mn_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_core_mn
#' @export
lsm_l_core_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_core_mn_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_core_mn_calc <- function(landscape){

    core_mean <- landscape %>%
        lsm_p_core_calc() %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "core area (mean)",
        value = core_mean$value
    )
}
