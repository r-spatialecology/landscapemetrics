#' Core area index distribution (class level)
#'
#' @description Standard deviation of core area index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions ???

#' Equals the standard deviation of the core area index of class i.
#' The core area index equals the percentage of a patch that is core area
#' \deqn{CAI_SD = sd(CAI[patch_i]}
#' \subsection{Units}{???}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cai_sd(landscape)
#'
#' @aliases lsm_c_cai_sd
#' @rdname lsm_c_cai_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_cai_sd <- function(landscape, directions) UseMethod("lsm_c_cai_sd")

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_c_cai_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_cai_sd_calc <- function(landscape, directions = 8){
    cai_sd <- landscape %>%
        lsm_p_cai_calc(directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = cai_sd$class,
        id = as.integer(NA),
        metric = "core area index (sd)",
        value = cai_sd$value
    )
}
