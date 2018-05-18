#' Total class area (class level)
#'
#' @description Total area of class (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ta(landscape)
#'
#' @aliases lsm_c_ta
#' @rdname lsm_c_ta
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_ta <- function(landscape) UseMethod("lsm_c_ta")

#' @name lsm_c_ta
#' @export
lsm_c_ta.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ta_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ta
#' @export
lsm_c_ta.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ta_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ta
#' @export
lsm_c_ta.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ta_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ta
#' @export
lsm_c_ta.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_ta_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_ta_calc <- function(landscape) {
    total_area <- landscape %>%
        lsm_p_area() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value))

    tibble::tibble(
        level = "class",
        class = total_area$class,
        id = as.integer(NA),
        metric = "total area",
        value = total_area$value
    )
}
