#' Number of core areas distribution (class level)
#'
#' @description Coeffiecent of variation of number of core areas (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions ???
#'
#' @details
#' Equals the coeffiecent of variation of number of core area of class i.
#' A core area is a 'patch within the patch' without any edge cells. In other words,
#' the number of patches within the patch that only have neighbouring cells of the same type
#' \deqn{NCORE_CV = cv(NCORE[patch_i])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ncore_cv(landscape)
#'
#' @aliases lsm_c_ncore_cv
#' @rdname lsm_c_ncore_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_ncore_cv <- function(landscape, directions) UseMethod("lsm_c_ncore_cv")

#' @name lsm_c_ncore_cv
#' @export
lsm_c_ncore_cv.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_cv_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ncore_cv
#' @export
lsm_c_ncore_cv.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_cv_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ncore_cv
#' @export
lsm_c_ncore_cv.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_cv_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ncore_cv
#' @export
lsm_c_ncore_cv.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_c_ncore_cv_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_ncore_cv_calc <- function(landscape, directions = 8){
    ncore_sd <- landscape %>%
        lsm_p_ncore_calc(directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = ncore_sd$class,
        id = as.integer(NA),
        metric = "number of core areas (cv)",
        value = ncore_sd$value
    )
}
