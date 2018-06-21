#' Relative patch richness (landscape level)
#'
#' @description Relative patch richness (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param classes_max ???
#' @details
#' Relative patch richness equals the number of classes divided by the (potential)
#' maximum number of classes. The maximum number of classes needs to be specified
#' \deqn{RPR = (number of classes / potential number of classes) * 100}
#' \subsection{Units}{Percentage}
#' \subsection{Ranges}{0 < RPR < 100}
#' \subsection{Behaviour}{RPR approaches RPR = 0 when only one class type is present,
#' but the maximum number of classes is large. RPR approaches 100 when the number of
#' present class types approaches the maximum number of classes}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_rpr(landscape, classes_max = 5)
#'
#' @aliases lsm_l_rpr
#' @rdname lsm_l_rpr
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_rpr <- function(landscape, classes_max) UseMethod("lsm_l_rpr")


#' @name lsm_l_rpr
#' @export
lsm_l_rpr.RasterLayer <- function(landscape, classes_max = NULL) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_rpr_calc,
                   classes_max = classes_max, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_rpr
#' @export
lsm_l_rpr.RasterStack <- function(landscape, classes_max = NULL) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_rpr_calc,
                   classes_max = classes_max, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_rpr
#' @export
lsm_l_rpr.RasterBrick <- function(landscape, classes_max = NULL) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_rpr_calc,
                   classes_max = classes_max, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#" @name lsm_l_rpr
#" @export
lsm_l_rpr.list <- function(landscape, classes_max = NULL) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_rpr_calc,
                   classes_max = classes_max, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_rpr_calc <- function(landcape, classes_max = NULL) {


    if(is.null(classes_max)) {
        warning("RPR = NA for classes_max=NULL")
        rpr <- NA
    }

    else {
        rpr <- landscape %>%
            lsm_l_pr_calc() %>%
            dplyr::mutate(value = value / classes_max * 100) %>%
            dplyr::pull(value)
    }

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.numeric(NA),
        metric = "relative patch richness",
        value = as.double(rpr)
    )
}
