#' Largest patch index (landscape level)
#'
#' @description Area of the larges patch divided by the total area (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_lpi(landscape)
#' lsm_l_lpi(landscape_stack)
#'
#' @aliases lsm_l_lpi
#' @rdname lsm_l_lpi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_lpi <- function(landscape) UseMethod("lsm_l_lpi")

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_lpi_calc <- function(landscape) {

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
        }) %>%
        dplyr::filter(value == max(value)) %>%
        dplyr::mutate(id = NA)
}
