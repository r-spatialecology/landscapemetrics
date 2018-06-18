#' Patch area distribution (landscape level)
#'
#' @description Mean patch size (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#'#' @details
#' Equals the standard deviation of patch area of all patches in the landscape
#' \deqn{AREA_SD = sd(AREA[patch])}
#' \subsection{Units}{Hectares}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#' @return tibble
#'
#' @return tibble
#'
#' @examples
#' lsm_l_area_sd(landscape)
#'
#' @aliases lsm_l_area_sd
#' @rdname lsm_l_area_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_area_sd <- function(landscape) UseMethod("lsm_l_area_sd")

#' @name lsm_l_area_sd
#' @export
lsm_l_area_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_area_sd
#' @export
lsm_l_area_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_area_sd
#' @export
lsm_l_area_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_area_sd
#' @export
lsm_l_area_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

# Not working yet!
lsm_l_area_sd_calc <- function(landscape){
    area_sd <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "patch area (sd)",
        value = area_sd$value
    )
}


