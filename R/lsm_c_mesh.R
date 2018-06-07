#' Effective mesh size (class level)
#'
#' @description Effective mesh size (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_c_mesh(landscape)
#'
#' @aliases lsm_c_mesh
#' @rdname lsm_c_mesh
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_mesh <- function(landscape) UseMethod("lsm_c_mesh")

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_mesh_calc <- function(landscape) {

    total_area <- lsm_l_ta(landscape)

    mesh <- landscape %>%
        lsm_p_area() %>%
        dplyr::mutate(value = value ^ 2) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::mutate(value = (value / total_area$value) * (1 / 10000))

    tibble::tibble(
        level = "class",
        class = mesh$class,
        id = as.integer(NA),
        metric = "effective mesh size",
        value = mesh$value
    )
}
