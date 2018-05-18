#' Edge Density
#'
#' @description Edge Density
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ed(landscape)
#' lsm_c_ed(landscape_stack)
#' lsm_c_ed(raster::as.list(landscape_stack))
#'
#' @aliases lsm_c_ed
#' @rdname lsm_c_ed
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_ed <- function(landscape)
    UseMethod("lsm_c_ed")

#' @name lsm_c_ed
#' @export
lsm_c_ed.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ed
#' @export
lsm_c_ed.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ed
#' @export
lsm_c_ed.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ed
#' @export
lsm_c_ed.list <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_ed_calc <- function(landscape) {

    total_edge_length <- lsm_p_perim(landscape) %>%
        dplyr::group_by(class) %>%
        dplyr::summarize(value = sum(value, na.rm = TRUE))

    landscape_area <- ncell(landscape)

    ed <- (total_edge_length / landscape_area) * 10000

    tibble::tibble(
        level = "class",
        class = raster::unique(landscape),
        id = as.integer(NA),
        metric = "edge density",
        value = ed$value
    )
}
