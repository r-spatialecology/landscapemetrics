#' Total core area (class level)
#'
#' @description Total core area (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions ???
#'
#' @details
#' Equals the area within a patch that is not on the edge of the patch of class i.
#' In other words, the area of a patch that has only neighbouring cells of the same type
#' of class i
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE >= 0}
#' \subsection{Behaviour}{CORE increases without limit as patch areas increase
#' and patch shapes simplify. CORE = 0 when every cell in every patch of class i
#' is an edge}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_core(landscape)
#'
#' @aliases lsm_c_core
#' @rdname lsm_c_core
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_core <- function(landscape, directions) UseMethod("lsm_c_core")

#' @name lsm_c_core
#' @export
lsm_c_core.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core
#' @export
lsm_c_core.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_core
#' @export
lsm_c_core.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_core
#' @export
lsm_c_core.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_c_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_core_calc <- function(landscape, directions = 8){
    core_area <- landscape %>%
        lsm_p_core_calc(directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = core_area$class,
        id = as.integer(NA),
        metric = "core area",
        value = core_area$value
    )
}
