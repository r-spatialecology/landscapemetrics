#' Number of patches
#'
#' @description Number of patches
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_np(landscape)
#'
#' @aliases lsm_l_np
#' @rdname lsm_l_np
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_np <- function(landscape) UseMethod("lsm_l_np")

#' @name lsm_l_np
#' @export
lsm_l_np.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_np_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_np
#' @export
lsm_l_np.RasterStack <- function(landscape) {
    purrr::map_dfr(
        raster::as.list(landscape),
        .f = lsm_l_np_calc,
        .id = "layer"
    ) %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_np
#' @export
lsm_l_np.RasterBrick <- function(landscape) {
    purrr::map_dfr(
        raster::as.list(landscape),
        .f = lsm_l_np_calc,
        .id = "layer"
    ) %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_np
#' @export
lsm_l_np.list <- function(landscape) {
    purrr::map_dfr(
        raster::as.list(landscape),
        .f = lsm_l_np_calc,
        .id = "layer"
    ) %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_np_calc <- function(landscape) {

    n_patches <- lsm_c_np_calc(landscape)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "number of patches",
        value = sum(n_patches$value)
    )

}
