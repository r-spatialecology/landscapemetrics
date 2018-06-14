#' Shannon's diversity index (landscape level)
#'
#' @description Shannon's diversity index (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Shannon's diversity index equals minus the sum of class proportions multipled by the natural
#' logarithm of class proportions
#' \deqn{SHDI = sum(proportion[patch_i] * log(proportion[patch_i]))}
#' \subsection{Units}{None}
#' \subsection{Range}{SHDI >= 0}
#' \subsection{Behaviour}{SHDI = 0 when only one patch and class is present and increases
#' without limit as the number of classes increases while the proportions are
#' equally distributed}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_shdi(landscape)
#'
#' @aliases lsm_l_shdi
#' @rdname lsm_l_shdi
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_shdi <- function(landscape)
    UseMethod("lsm_l_shdi")

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.RasterLayer = function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shdi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.RasterStack = function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shdi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.RasterBrick = function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_shdi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_shdi
#' @export
lsm_l_shdi.list = function(landscape) {
    purrr::map_dfr(landscape, lsm_l_shdi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_shdi_calc = function(landscape) {
    area <- raster::ncell(landscape)

    p <- landscape %>%
        raster::values() %>%
        table() / area

    H <- tibble::tibble(
        level = 'landscape',
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "Shannon's diversity index",
        value = sum(-p * log(p, exp(1)), na.rm = TRUE)
    )
    H
}
