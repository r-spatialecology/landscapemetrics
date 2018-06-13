#' Patch richness density (lanscape level)
#'
#' @description Patch richness density (lanscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Number of classes in the landscape divided by the total area.
#' This is a very simple measure of composition and comparable among landscapes
#' with different total area
#' \deqn{PRD = number of classes / total area}
#' \subsection{Units}{Number per hectare}
#' \subsection{Range}{PRD > 0}
#' \subsection{Behaviour}{PRD approaches PRD = 0 when only one class is present and
#' the landscape area increases. PRD increases as the number of classes increases and
#' the total area decreases}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_prd(landscape)
#'
#' @aliases lsm_l_prd
#' @rdname lsm_l_prd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_prd <- function(landscape) UseMethod("lsm_l_prd")

#' @name lsm_l_prd
#' @export
lsm_l_prd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_prd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_prd
#' @export
lsm_l_prd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_prd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_prd
#' @export
lsm_l_prd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_prd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_prd
#' @export
lsm_l_prd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_prd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_prd_calc <- function(landscape) {

    area_landscape <- lsm_l_ta(landscape)

    prd <- landscape %>%
        lsm_l_pr() %>%
        dplyr::mutate(value = value / area_landscape$value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "patch richness density",
        value = prd$value
    )
}
