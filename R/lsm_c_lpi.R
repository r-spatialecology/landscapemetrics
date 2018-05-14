#' Largest patch index (class level)
#'
#' @description Area of the larges patch divided by the total area (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_lpi(landscape)
#' lsm_c_lpi(landscape_stack)
#'
#' @aliases lsm_c_lpi
#' @rdname lsm_c_lpi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_lpi <- function(landscape) UseMethod("lsm_c_lpi")

#' @name lsm_c_lpi
#' @export
lsm_c_lpi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lpi
#' @export
lsm_c_lpi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lpi
#' @export
lsm_c_lpi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lpi
#' @export
lsm_c_lpi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_lpi_calc <- function(landscape) {

    total_area <- raster::ncell(landscape) * prod(raster::res(landscape))

    landscape %>%
        cclabel() %>%
        purrr::map2_dfr(.x = ., .y = seq_along(.), .f = function(x, y){

            lpi <- raster::values(x) %>%
                table(useNA = "no") %>%
                magrittr::multiply_by(prod(raster::res(landscape))) %>%
                max() %>%
                magrittr::divide_by(total_area) %>%
                magrittr::multiply_by(100)

            tibble::tibble(
                level = "class",
                id = as.integer(y),
                metric = "largest patch index",
                value = lpi
            )
        })
}
