#' Number of patches (class level)
#'
#' @description Number of patches (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_np(landscape)
#'
#' @aliases lsm_c_np
#' @rdname lsm_c_np
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_np <- function(landscape) UseMethod("lsm_c_np")

#' @name lsm_c_np
#' @export
lsm_c_np.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_np_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_np
#' @export
lsm_c_np.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_np_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_np
#' @export
lsm_c_np.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_np_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_np
#' @export
lsm_c_np.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_np_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_np_calc <- function(landscape){
    landscape %>%
        cclabel() %>%
        raster::as.list() %>%
        purrr::map2_dfr(.x = ., .y = 1:length(.), .f = function(x, y){
            tibble::tibble(
                level = "class",
                class = y,
                id = as.integer(NA),
                metric = "number of patches",
                value = length(unique(raster::values(x)[!is.na(raster::values(x))]))
            )
        })
}
