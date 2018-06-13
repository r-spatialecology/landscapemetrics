#' Patch richness (lanscape level)
#'
#' @description Patch richness (lanscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Number of classes in the landscape. This is a very simple measure of composition.
#' \subsection{Units}{None}
#' \subsection{Range}{PR >= 1}
#' \subsection{Behaviour}{PR = 1 when only 1 class is present and increases without
#' limit as the number of classes increases}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_pr(landscape)
#'
#' @aliases lsm_l_pr
#' @rdname lsm_l_pr
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export

lsm_l_pr <- function(landscape) UseMethod("lsm_l_pr")

#' @name lsm_l_pr
#' @export
lsm_l_pr.RasterLayer = function(landscape){
    purrr::map_dfr(raster::as.list(landscape), lsm_l_pr_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_pr
#' @export
lsm_l_pr.RasterStack = function(landscape){
    purrr::map_dfr(raster::as.list(landscape), lsm_l_pr_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_pr
#' @export
lsm_l_pr.RasterBrick = function(landscape){
    purrr::map_dfr(raster::as.list(landscape), lsm_l_pr_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_pr
#' @export
lsm_l_pr.list = function(landscape){
    purrr::map_dfr(landscape, lsm_l_pr_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_pr_calc = function(landscape){
    richness <- length(raster::unique(landscape))

    tibble::tibble(
        level = 'landscape',
        class = as.integer(NA),
        id = as.integer(NA),
        metric = 'patch richness',
        value = richness)

}
