#' Effective mesh size (landscape level)
#'
#' @description Effective mesh size (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_l_mesh(landscape)
#'
#' @aliases lsm_l_mesh
#' @rdname lsm_l_mesh
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_mesh <- function(landscape) UseMethod("lsm_l_mesh")

#' @name lsm_l_mesh
#' @export
lsm_l_mesh.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_mesh
#' @export
lsm_l_mesh.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_mesh
#' @export
lsm_l_mesh.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_mesh
#' @export
lsm_l_mesh.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_mesh_calc <- function(landscape) {

    total_area <- lsm_l_ta(landscape)

    mesh <- landscape %>%
        lsm_p_area() %>%
        dplyr::mutate(value = value ^ 2) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::mutate(value = (value / total_area$value) * (1 / 10000))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "effective mesh size",
        value = mesh$value
    )
}
