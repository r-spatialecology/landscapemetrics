#' Perimeter-area ratio distribution (class level)
#'
#' @description Standart deviation of perimeter-area ratio (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details Standard deviation of the perimeter-area ratio of all patches of class i.
#' PARA equals the ration of patch perimeter and patch area. It is a simple measure of complexity
#' \deqn{PARA_SD = sd(PARA[patch_i]}
#' \subsection{Units}{None}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_para_sd(landscape)
#'
#' @aliases lsm_c_para_sd
#' @rdname lsm_c_para_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_para_sd <- function(landscape) UseMethod("lsm_c_para_sd")

#' @name lsm_c_para_sd
#' @export
lsm_c_para_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_para_sd
#' @export
lsm_c_para_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_para_sd
#' @export
lsm_c_para_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_para_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_para_sd
#' @export
lsm_c_para_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_para_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_para_sd_calc <- function(landscape){

    para_sd <- landscape %>%
        lsm_p_para() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = para_sd$class,
        id = as.integer(NA),
        metric = "perimeter-area-ratio (sd)",
        value = para_sd$value
    )
}
