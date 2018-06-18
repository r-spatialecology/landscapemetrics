#' Euclidean Nearest Neighbor Distance Distribution (class level)
#'
#' @description Standard deviation Euclidean Nearest Neighbor Distance (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the standard deviation of the eclidean nearest neighbor distance of class i.
#' ENN equals the distance the the nearest neigbouring patch of the same patch type
#' (shortest edge-to-edge distance)
#' \deqn{ENN_SD = sd(ENN[patch_i])}
#' \subsection{Units}{Meters}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_enn_sd(landscape)
#'
#' @aliases lsm_c_enn_sd
#' @rdname lsm_c_enn_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_enn_sd <- function(landscape) UseMethod("lsm_c_enn_sd")

#' @name lsm_c_enn_sd
#' @export
lsm_c_enn_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_enn_sd
#' @export
lsm_c_enn_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_enn_sd
#' @export
lsm_c_enn_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_enn_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_enn_sd
#' @export
lsm_c_enn_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_enn_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}


lsm_c_enn_sd_calc <- function(landscape) {

    enn_sd  <- lsm_p_enn_calc(landscape) %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = seq_len(nrow(enn_mn)),
        id = as.integer(NA),
        metric = "euclidean nearest neighbor distance distribution (sd)",
        value = enn_sd$value
    )

}
