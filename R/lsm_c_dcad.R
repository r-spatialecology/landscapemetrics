#' Disjunct core area density (class level)
#
#' @description Disjunct core area density (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Disjunct core area density equals the sum of number of core areas  of
#' class i divided by the total area multiplied by 100 and 10 000. In other words,
#' it is the number of core areas relative to the total area scaled to 100 hectares (???).
#' The measure is relative and scaled to 100 hecatres (???) it is comparable among
#' landscapes with different total areas
#' \deqn{DCAD = (sum(ncore[patch_i]) / total area) * 100 * 10 000}
#' \subsection{Units}{Number per 100 hectares}
#' \subsection{Range}{DCAD >= 0 \cr DCAD = 0 when TCA = 0, i.e. every cell in patches
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
lsm_c_dcad <- function(landscape) UseMethod("lsm_c_dcad")

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_dcad_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_dcad_calc <- function(landscape){

    total_area <- lsm_l_ta(landscape)

    dcad <- lsm_c_ncore(landscape) %>%
        dplyr::mutate(value = value / total_area$value  * 100  * 10000) # Correct unit?

    tibble::tibble(
        level = "class",
        class = dcad$class,
        id = as.integer(NA),
        metric = "disjunct core area density",
        value = dcad$value
    )
}
