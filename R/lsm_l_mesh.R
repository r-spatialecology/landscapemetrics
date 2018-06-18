#' Effective Mesh Size (landscape level)
#'
#' @description Effective Mesh Size (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The effective mesh size equals the sum of patch areas of all patches in the landscape
#' squared divided by the total area
#' \deqn{MESH = (sum(area[patch]) ^ 2) / total area}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{ratio of cell size to total area (???) <= MESH <= total area}
#' \subsection{Behaviour}{MESH equals the lower limit when every cell is a patch and
#' increases when only one patch is present}
#'
#' @return tibble
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

    total_area <- lsm_l_ta_calc(landscape)

    mesh <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = value ^ 2) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(value = (value / total_area$value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "effective mesh size",
        value = mesh$value
    )
}
