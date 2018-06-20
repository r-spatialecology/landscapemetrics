#'  Total area of core areas (landscape level)
#'
#' @description Area of corea area (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the area within a patch that is not on the edge of all patches in the landscape.
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
#' lsm_l_tca(landscape)
#'
#' @aliases lsm_l_tca
#' @rdname lsm_l_tca
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_tca <- function(landscape) UseMethod("lsm_l_tca")


#' @name lsm_l_tca
#' @export
lsm_l_tca.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_tca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_tca
#' @export
lsm_l_tca.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_tca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_tca
#' @export
lsm_l_tca.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_tca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_tca
#' @export
lsm_l_tca.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_tca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_tca_calc <- function(landscape) {

    total_core_area <- landscape %>%
        lsm_p_core_calc() %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "total core area",
        value = total_core_area$value
    )
}
