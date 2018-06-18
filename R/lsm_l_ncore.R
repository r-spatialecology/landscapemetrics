#' Number of core areas (landscape level)
#'
#' @description Number of core areas (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions ???
#'
#' @details
#' Number of core areas equals the sum of number of core areas of all patches in the
#' landscape. Called number of disjunct core areas in FRAGSTATS. A core area is a
#' 'patch within the patch' without any edge cells. In other words, the number of
#' patches within the patch that only have neighbouring cells of the same type
#' \deqn{NCORE = sum(ncore[patch])}
#' \subsection{Units}{None}
#' \subsection{Range}{NCORE >= 0}
#' \subsection{Behaviour}{NCORE = 0 when CORE = 0, i.e. every cell in patches of class i is
#' an edge. NCORE increases without limit as core areas become more present, i.e. patches
#' becoming larger and less complex}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ncore(landscape)
#'
#' @aliases lsm_l_ncore
#' @rdname lsm_l_ncore
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_ncore <- function(landscape, directions) UseMethod("lsm_l_ncore")

#' @name lsm_l_ncore
#' @export
lsm_l_ncore.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ncore
#' @export
lsm_l_ncore.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ncore
#' @export
lsm_l_ncore.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ncore
#' @export
lsm_l_ncore.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_l_ncore_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_ncore_calc <- function(landscape, directions = 8){

    ncore <- landscape %>%
        lsm_p_ncore_calc(directions = directions) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class =  as.integer(NA),
        id = as.integer(NA),
        metric = "number of disjunct core areas",
        value = ncore$value
    )
}
