#' Total area (landscape level)
#'
#' @description Total area of class (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Total area equals the sum of the area of all patches in the landscape
#' \deqn{TA = sum(area[patch])}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CA > 0}
#' \subsection{Behaviour}{TA increases without limit as landscape size increases}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ta(landscape)
#'
#' @aliases lsm_l_ta
#' @rdname lsm_l_ta
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_ta <- function(landscape) UseMethod("lsm_l_ta")


#' @name lsm_l_ta
#' @export
lsm_l_ta.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ta_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ta
#' @export
lsm_l_ta.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ta_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ta
#' @export
lsm_l_ta.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ta_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ta
#' @export
lsm_l_ta.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_ta_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_ta_calc <- function(landscape) {

    total_area <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "total area",
        value = total_area$value
    )
}
