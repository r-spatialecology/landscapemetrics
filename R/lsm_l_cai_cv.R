#' Core area index distribution (landscape level)
#'
#' @description Coefficient of variation of core area index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions ???
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
lsm_l_cai_cv <- function(landscape, directions) UseMethod("lsm_l_cai_cv")

#' @name lsm_l_cai_cv
#' @export
lsm_l_cai_cv.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_cai_cv_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_cai_cv
#' @export
lsm_l_cai_cv.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_cai_cv_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_cai_cv
#' @export
lsm_l_cai_cv.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_cai_cv_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_cai_cv
#' @export
lsm_l_cai_cv.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_l_cai_cv_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_cai_cv_calc <- function(landscape, directions = 8){

    cai_cv <- landscape %>%
        lsm_p_cai_calc(directions = directions) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "core area index (cv)",
        value = cai_cv$value
    )
}
