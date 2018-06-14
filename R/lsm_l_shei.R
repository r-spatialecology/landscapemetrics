#' Shannons's evenness index (landscape level)
#'
#' @description Shannons's evenness index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Shannon's evenness index equals minus the sum of class proportions multipled by the natural
#' logarithm of class proportions divided by the natural logarithm of class types
#' \deqn{SHEI = sum(proportion[patch_i] * log(proportion[patch_i])) / log(number of classes)}
#' \subsection{Units}{None}
#' \subsection{Range}{0 <= SHEI < 1}
#' \subsection{Behaviour}{Shannons's evenness index approaches SHEI = 0 when only one patch and
#' class is present and approaches SHEI = 1 when the number of class types increases while the
#' proportions are equally distributed}
#'
#' @return tibble
#'
#'
#' @examples
#' lsm_l_shei(landscape)
#'
#' @aliases lsm_l_shei
#' @rdname lsm_l_shei
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_shei <- function(landscape) UseMethod("lsm_l_shei")

#' @name lsm_l_shei
#' @export
lsm_l_shei.RasterLayer = function(landscape){
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shei
#' @export
lsm_l_shei.RasterStack = function(landscape){
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shei
#' @export
lsm_l_shei.RasterBrick = function(landscape){
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shei
#' @export
lsm_l_shei.list = function(landscape){
    purrr::map_dfr(landscape, lsm_l_shei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_shei_calc = function(landscape){
    area <- raster::ncell(landscape)

    p <- landscape %>%
        raster::values() %>%
        table() / area

    E <- tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "Shannon's evenness index",
        value = sum(-p * log(p, exp(1)), na.rm = TRUE) / log(length(p), exp(1))
    )
    E
}
