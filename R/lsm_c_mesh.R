#' Effective Mesh Size (class level)
#'
#' @description Effective Mesh Size (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The effective mesh size equals the sum of patch areas of class i squared
#' divided by the total area divided by 10 000 to convert to hectares (???)
#' \deqn{MESH = (sum(area[patch_i]) ^ 2) / total area}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{ratio of cell size to total area (???) <= MESH <= total area}
#' \subsection{Behaviour}{MESH equals the lower limit when every cell is a patch and
#' increases when only one patch is present}
#'
#' @return tibble
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

    total_area <- lsm_l_ta(landscape) %>%
        dplyr::mutate(value = value * 10000)

    mesh <- landscape %>%
        lsm_p_area() %>%
        dplyr::mutate(value = (value * 10000) ^ 2) %>%
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
