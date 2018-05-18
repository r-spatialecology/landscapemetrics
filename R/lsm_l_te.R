#'  Total Edge (TE, landscape scale)
#'
#' @description  Total Edge
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_te(landscape)
#'
#' @aliases lsm_l_te
#' @rdname lsm_l_te
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_te <- function(landscape) UseMethod("lsm_l_te")

#' @name lsm_l_te
#' @export
lsm_l_te.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_te
#' @export
lsm_l_te.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_te
#' @export
lsm_l_te.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_te
#' @export
lsm_l_te.list <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_te_calc <- function(landscape){

    total_edge <- lsm_c_te_calc(landscape)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "euclidean nearest neighbor distance distribution (mean)",
        value = sum(total_edge$value)
    )

}
