#' Patch area distribution (class level)
#'
#' @description Mean patch area (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the mean patch area of class i.
#' \deqn{AREA_MN = mean(AREA[patch_i])}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_MN > 0}
#' \subsection{Behaviour}{AREA_MN increases without limit as the amount of the
#' class increases. AREA_MN = TA if only one class is present}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_area_mn(landscape)
#'
#' @aliases lsm_c_area_mn
#' @rdname lsm_c_area_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_area_mn <- function(landscape) UseMethod("lsm_c_area_mn")

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_area_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_area_mn_calc <- function(landscape){

    area_mean <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = area_mean$class,
        id = as.integer(NA),
        metric = "patch area (mean)",
        value = area_mean$value
    )
}
