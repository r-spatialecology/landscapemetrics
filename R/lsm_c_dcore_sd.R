#' Number of core areas distribution (class level)
#'
#' @description Standart deviation of number of core areas (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the standard deviation of number of core area of class i.
#' A core area is a 'patch within the patch' without any edge cells. In other words,
#' the number of patches within the patch that only have neighbouring cells of the same type
#' \deqn{DCORE_SD = sd(dcore[patch_i])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @importFrom stats sd
#'
#' @examples
#' lsm_c_dcore_sd(landscape)
#'
#' @aliases lsm_c_dcore_sd
#' @rdname lsm_c_dcore_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_dcore_sd <- function(landscape) UseMethod("lsm_c_dcore_sd")

#' @name lsm_c_dcore_sd
#' @export
lsm_c_dcore_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcore_sd_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcore_sd
#' @export
lsm_c_dcore_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcore_sd_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcore_sd
#' @export
lsm_c_dcore_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcore_sd_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcore_sd
#' @export
lsm_c_dcore_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_dcore_sd_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_dcore_sd_calc <- function(landscape){
    dcore_sd <- landscape %>%
        lsm_p_ncore_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = dcore_sd$class,
        id = as.integer(NA),
        metric = "number of core areas (sd)",
        value = dcore_sd$value
    )
}
