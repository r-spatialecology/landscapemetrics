#' Core area distribution (landscape level)
#'
#' @description Coeffiecent of variation of patch core area (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the coeffiecent of variation of the patch core area of all patches in the landscape.
#' The core area is the area within a patch that is not on the edge of the patch of class i.
#' In other words, the area of a patch that has only neighbouring cells of the same type
#' of class i
#' \deqn{CORE_CV = cv(core[patch])}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
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
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_core_cv <- function(landscape) UseMethod("lsm_l_core_cv")

#' @name lsm_l_core_cv
#' @export
lsm_l_core_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_core_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_core_cv
#' @export
lsm_l_core_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_core_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_core_cv
#' @export
lsm_l_core_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_core_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_core_cv
#' @export
lsm_l_core_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_core_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_core_cv_calc <- function(landscape){

    core_cv <- landscape %>%
        lsm_p_core() %>%
        dplyr::summarise(value = raster::cv(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "core area (cv)",
        value = core_cv$value
    )
}
