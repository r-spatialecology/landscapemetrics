#' Core area index distribution (class level)
#'
#' @description Mean of core area index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#'#' Equals the mean of the core area index of class i.
#' The core area index equals the percentage of a patch that is core area
#' \deqn{CAI_MN = mean(CAI[patch_i]}
#' \subsection{Units}{Percentage}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cai_mn(landscape)
#'
#' @aliases lsm_c_cai_mn
#' @rdname lsm_c_cai_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_cai_mn <- function(landscape) UseMethod("lsm_c_cai_mn")

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cai_mn
#' @export
lsm_c_cai_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_cai_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_cai_mn_calc <- function(landscape){
    cai_mean <- landscape %>%
        lsm_p_cai_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = cai_mean$class,
        id = as.integer(NA),
        metric = "core area index (mean)",
        value = cai_mean$value
    )
}
