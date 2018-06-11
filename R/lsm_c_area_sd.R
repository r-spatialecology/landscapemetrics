#' Patch area distribution (class level)
#'
#' @description Standard deviation of patch area (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the standard deviation of the patch area of class i
#' \deqn{AREA_SD = sd(area[patch_i])}
#' \subsection{Units}{Square meter (assuming that the input cellsize is in meter)}
#' \subsection{Range}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_area_sd(landscape)
#'
#' @aliases lsm_c_area_sd
#' @rdname lsm_c_area_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_area_sd <- function(landscape) UseMethod("lsm_c_area_sd")

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_area_sd_calc <- function(landscape){
    area_sd <- landscape %>%
        lsm_p_area() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = stats::sd(value))

    tibble::tibble(
        level = "class",
        class = area_sd$class,
        id = as.integer(NA),
        metric = "patch area (sd)",
        value = area_sd$value
    )
}
