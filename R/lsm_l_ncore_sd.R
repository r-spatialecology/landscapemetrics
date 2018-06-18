#' Number of core areas distribution (landscape level)
#'
#' @description Standart deviation of number of core areas (landscape level)
#' @param directions ???
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the standard deviation of number of core area of all patches in the landscape.
#' A core area is a 'patch within the patch' without any edge cells. In other words,
#' the number of patches within the patch that only have neighbouring cells of the same type
#' \deqn{NCORE_SD = sd(NCORE[patch_i])}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ncore_sd(landscape)
#'
#' @aliases lsm_l_ncore_sd
#' @rdname lsm_l_ncore_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_ncore_sd <- function(landscape, directions) UseMethod("lsm_l_ncore_sd")

#' @name lsm_l_ncore_sd
#' @export
lsm_l_ncore_sd.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ncore_sd
#' @export
lsm_l_ncore_sd.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ncore_sd
#' @export
lsm_l_ncore_sd.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ncore_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ncore_sd
#' @export
lsm_l_ncore_sd.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_l_ncore_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_ncore_sd_calc <- function(landscape, directions = 8){

    ncore_sd <- landscape %>%
        lsm_p_ncore_calc(directions = directions) %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "number of core areas (sd)",
        value = ncore_sd$value
    )
}
