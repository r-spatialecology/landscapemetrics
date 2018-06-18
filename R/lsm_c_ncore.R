#' Number of core areas (class level)
#'
#' @description Number of core areas (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param ... Specific arguments for certain functions, if not provided they fall back to default.

#' @details
#' Number of core areas equals the sum of number of core areas of class i.
#' Called number of disjunct core areas in FRAGSTATS. A core area is a 'patch within
#' the patch' without any edge cells. In other words, the number of patches within
#' the patch that only have neighbouring cells of the same type
#' \deqn{NCORE = sum(ncore[patch_i])}
#' \subsection{Units}{None}
#' \subsection{Range}{NCORE >= 0}
#' \subsection{Behaviour}{NCORE = 0 when CORE = 0, i.e. every cell in patches of class i is
#' an edge. NCORE increases without limit as core areas become more present, i.e. patches
#' becoming larger and less complex}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ncore(landscape)
#'
#' @aliases lsm_c_ncore
#' @rdname lsm_c_ncore
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_ncore <- function(landscape, ...) UseMethod("lsm_c_ncore")

#' @name lsm_c_ncore
#' @export
lsm_c_ncore.RasterLayer <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ncore
#' @export
lsm_c_ncore.RasterStack <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ncore
#' @export
lsm_c_ncore.RasterBrick <- function(landscape...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ncore_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ncore
#' @export
lsm_c_ncore.list <- function(landscape, ...) {
    purrr::map_dfr(landscape, lsm_c_ncore_calc,
                   ..., .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_ncore_calc <- function(landscape, ...){
    ncore <- landscape %>%
        lsm_p_ncore_calc(...) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = ncore$class,
        id = as.integer(NA),
        metric = "number of core areas",
        value = ncore$value
    )
}
