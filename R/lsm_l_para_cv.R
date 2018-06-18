#' Perimeter-area ratio distribution (landscape level)
#'
#' @description Coeffiecent of variation of perimeter-area ratio (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Coeffiecent of variation of the perimeter-area ratio of all patches in the landscape.
#' PARA equals the ration of patch perimeter and patch area. It is a simple measure of complexity
#' \deqn{PARA_CV = cv(PARA[patch]}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_para_cv(landscape)
#'
#' @aliases lsm_l_para_cv
#' @rdname lsm_l_para_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_para_cv <- function(landscape) UseMethod("lsm_l_para_cv")

#' @name lsm_l_para_cv
#' @export
lsm_l_para_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_para_cv
#' @export
lsm_l_para_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_para_cv
#' @export
lsm_l_para_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_para_cv
#' @export
lsm_l_para_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_para_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_para_cv_calc <- function(landscape){

    para_cv <- landscape %>%
        lsm_p_para_calc() %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "perimeter-area-ratio (cv)",
        value = para_cv$value
    )
}
