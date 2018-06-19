#' CAI_SD
#'
#' @description Standard deviation of the core area index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CAI_SD = sd(CAI[patch_{ij}]}
#' where \eqn{CAI[patch_{ij}]} is the core area index of each patch
#'
#' CAI_SD is a 'Core area metric'. The metric summarises each class
#' as the standard deviation of the core area index of all patches belonging to class i.
#' The core area index is the percentag of core area in relation to patch area
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{CAI_SD >= 0}
#' \subsection{Behaviour}{Increases as the variation of the core area indices increases}
#'
#' @seealso \code{\link{lsm_p_cai}} and \code{\link{sd}}
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
lsm_c_cai_sd <- function(landscape) UseMethod("lsm_c_cai_sd")

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_cai_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cai_sd
#' @export
lsm_c_cai_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_cai_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_cai_sd_calc <- function(landscape){
    cai_sd <- landscape %>%
        lsm_p_cai_calc() %>%
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
