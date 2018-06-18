#' Patch area distribution (landscape level)
#'
#' @description Coefficient of variation patch size (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#'#' @details
#' Equals the coeffiecent of variation of the area of all patches in the landscape
#' \deqn{AREA_CV = cv(AREA[patch])}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_area_cv(landscape)
#'
#' @aliases lsm_l_area_cv
#' @rdname lsm_l_area_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_area_cv <- function(landscape) UseMethod("lsm_l_area_cv")

#' @name lsm_l_area_cv
#' @export
lsm_l_area_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_area_cv
#' @export
lsm_l_area_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_area_cv
#' @export
lsm_l_area_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_area_cv
#' @export
lsm_l_area_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_area_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

# Not working yet!
lsm_l_area_cv_calc <- function(landscape){
    area_cv <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "patch area (cv)",
        value = area_cv$value
    )
}


