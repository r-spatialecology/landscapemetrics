#' Relative patch ritchness
#'
#' @description Number of different classes divided by (potential) maximum number of classes
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param classes_max Maximum number of classes
#'
#' @return Value >= 1
#'
#' @examples
#' lsm_l_rpr(landscape)
#'
#' @aliases lsm_l_rpr
#' @rdname lsm_l_rpr
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_rpr <- function(landscape, classes_max = NULL) UseMethod("lsm_l_rpr")


#' @name lsm_l_rpr
#' @export
lsm_l_rpr.RasterLayer <- function(landscape, classes_max) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_rpr_calc,
                   classes_max = classes_max, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_rpr
#' @export
lsm_l_rpr.RasterStack <- function(landscape, classes_max) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_rpr_calc,
                   classes_max = classes_max, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_rpr
#' @export
lsm_l_rpr.RasterBrick <- function(landscape, classes_max) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_rpr_calc,
                   classes_max = classes_max, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#" @name lsm_l_rpr
#" @export
lsm_l_rpr.list <- function(landscape, classes_max) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_rpr_calc,
                   classes_max = classes_max, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_rpr_calc <- function(landcape, classes_max) {

    if(is.null(classes_max)) {
        warning("classes_max is NULL - returning NA")
        tibble::tibble(
            level = "landscape",
            id = as.numeric(NA),
            metric = "relative patch richness",
            value = 0
        )
    }

    else {
        tibble::tibble(
        level = "landscape",
        id = as.numeric(NA),
        metric = "relative patch richness",
        value = length(raster::unique(landscape)) / classes_max * 100
        )
    }
}
