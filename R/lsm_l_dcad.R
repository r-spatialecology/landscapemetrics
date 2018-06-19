#' Disjunct core area density (landscape level)
#
#' @description Disjunct core area density (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Disjunct core area density equals the sum of number of core areas of the
#' landscape divided by the total area. In other words, it is the number of core areas
#' relative to the total area. The measure is relative and therefore comparable among
#' landscapes with different total areas
#' \deqn{DCAD = sum(ncore[patch]) / total area}
#' \subsection{Units}{Number per hectares (FRAGSTATS has per 100 ha)}
#' \subsection{Range}{DCAD >= 0}
#' \subsection{Behaviour}{DCAD = 0 when CORE = 0, i.e. every cell in patches
#' of class i is an edge. DCAD increases without limit as core areas become more
#' present, i.e. patches becoming larger and less complex}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_dcad(landscape)
#'
#' @aliases lsm_l_dcad
#' @rdname lsm_l_dcad
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_dcad <- function(landscape) UseMethod("lsm_l_dcad")

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_dcad_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_dcad_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_dcad_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_dcad
#' @export
lsm_l_dcad.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_dcad_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_dcad_calc <- function(landscape){

    total_area <- lsm_l_ta_calc(landscape)

    dcad <- landscape %>%
        lsm_p_ncore() %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(value = value / total_area$value) # Correct unit?

    tibble::tibble(
        level = "class",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "disjunct core area density",
        value = dcad$value
    )
}
