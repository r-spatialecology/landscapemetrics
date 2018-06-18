#' Disjunct core area density (class level)
#
#' @description Disjunct core area density (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions ???

#' @details
#' Disjunct core area density equals the sum of number of core areas of
#' class i divided by the total area. In other words, it is the number of core areas
#' relative to the total area. The measure is relative and therefore comparable among
#' landscapes with different total areas
#' \deqn{DCAD = sum(ncore[patch_i]) / total area}
#' \subsection{Units}{Number per hectares (FRAGSTATS has per 100 ha)}
#' \subsection{Range}{DCAD >= 0}
#' \subsection{Behaviour}{DCAD = 0 when CORE = 0, i.e. every cell in patches
#' of class i is an edge. DCAD increases without limit as core areas become more
#' present, i.e. patches becoming larger and less complex}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_dcad(landscape)
#'
#' @aliases lsm_c_dcad
#' @rdname lsm_c_dcad
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_dcad <- function(landscape, directions) UseMethod("lsm_c_dcad")

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcad_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcad_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcad_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_c_dcad_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_dcad_calc <- function(landscape, directions = 8){

    total_area <- lsm_l_ta_calc(landscape)

    dcad <- landscape %>%
        lsm_c_ncore_calc(directions = directions) %>%
        dplyr::mutate(value = value / total_area$value) # Correct unit?

    tibble::tibble(
        level = "class",
        class = dcad$class,
        id = as.integer(NA),
        metric = "disjunct core area density",
        value = dcad$value
    )
}
