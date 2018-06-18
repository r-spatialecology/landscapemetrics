#' Core area index distribution (landscape level)
#'
#' @description Coefficient of variation of core area index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param ... Specific arguments for certain functions, if not provided they fall back to default.
#'
#' @details
#' Equals the coeffiecent of variation of the core area index of all patches in the landscape.
#' The core area index equals the percentage of a patch that is core area
#' \deqn{CAI_CV = cv(CAI[patch]}
#' \subsection{Units}{???}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_cai_cv(landscape)
#'
#' @aliases lsm_l_cai_cv
#' @rdname lsm_l_cai_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_cai_cv <- function(landscape, ...) UseMethod("lsm_l_cai_cv")

#' @name lsm_l_cai_cv
#' @export
lsm_l_cai_cv.RasterLayer <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_cai_cv_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_cai_cv
#' @export
lsm_l_cai_cv.RasterStack <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_cai_cv_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_cai_cv
#' @export
lsm_l_cai_cv.RasterBrick <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_cai_cv_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_cai_cv
#' @export
lsm_l_cai_cv.list <- function(landscape, ...) {
    purrr::map_dfr(landscape, lsm_l_cai_cv_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_cai_cv_calc <- function(landscape, ...){

    cai_cv <- landscape %>%
        lsm_p_cai_calc(...) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "core area index (cv)",
        value = cai_cv$value
    )
}
