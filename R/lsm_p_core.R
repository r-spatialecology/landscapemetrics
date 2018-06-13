#'  Core area (patch level)
#'
#' @description Area of core area of patch (patch level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions ...
#'
#' @details
#' Equals the area within a patch that is not on the edge of the patch. In other words,
#' the area of a patch that has only neighbouring cells of the same type
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE >= 0}
#' \subsection{Behaviour}{Increases without limit as patch area increases
#' and patch shape simplifies. CORE = 0 when every cell in the patch is an edge}
#'
#' @return tibble
#'
#' @importFrom stats na.omit
#'
#' @examples
#' lsm_p_core(landscape)
#'
#' @aliases lsm_p_core
#' @rdname lsm_p_core
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_core <- function(landscape, directions) UseMethod("lsm_p_core")

#' @name lsm_p_core
#' @export
lsm_p_core.RasterLayer <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_core
#' @export
lsm_p_core.RasterStack <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_core
#' @export
lsm_p_core.RasterBrick <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_core
#' @export
lsm_p_core.list <- function(landscape, directions = 4) {
    purrr::map_dfr(landscape, lsm_p_core_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_core_calc <- function(landscape, directions){

    core_area <- landscape %>%
        cclabel() %>%
        unname() %>%
        purrr::map_dfr(function(landscape_patch) {
            landscape_patch %>%
                raster::values() %>%
                stats::na.omit() %>%
                unique() %>%
                sort() %>%
                purrr::map_dfr(function(patch_id) {
                    landscape_patch[landscape_patch != patch_id |
                                        is.na(landscape_patch)] <- -999
                    core_cells <- raster::Which(landscape_patch == patch_id, cells = T) %>%
                        purrr::map_dbl(function(cell_id) {
                            adjacent_cells <- raster::adjacent(landscape_patch,
                                                               cells = cell_id,
                                                               directions = directions,
                                                               pairs = FALSE)
                            ifelse(all(landscape_patch[adjacent_cells] == patch_id), cell_id, NA)
                        }) %>%
                        na.omit() %>%
                        length()

                    tibble::tibble(
                        id = NA,
                        value = (core_cells * prod(raster::res(landscape)) / 10000)
                    )
                })
        }, .id = "class")

    tibble::tibble(
        level = "patch",
        class = as.integer(core_area$class),
        id = seq_len(nrow(core_area)),
        metric = "core area",
        value = core_area$value
    )
}
