#' PARA_SD (class level)
#'
#' @description Standard deviation perimeter-area ratio (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{PARA_{SD} = sd(PARA[patch_{ij}]}
#' where \eqn{PARA[patch_{ij}]} is the perimeter area ratio of each patch.
#'
#' PARA_SD is a 'Shape metric'. It summarises each class as the standard deviation of
#' each patch belonging to class i. The perimeter-area ratio describes the patch complexity
#' in a straightforward way. However, because it is not standarised to a certain shape
#' (e.g. a square), it is not scale independent, meaning that increasing the patch size
#' while not changing the patch form will change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA_SD >= 0}
#' \subsection{Behaviour}{Equals PARA_SD = 0 if the perimeter-area ratio is identical for
#' all patches. Increases, without limit, as the variation of the perimeter-area ratio
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_para}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_para_mn}},
#' \code{\link{lsm_c_para_cv}}, \cr
#' \code{\link{lsm_l_para_mn}},
#' \code{\link{lsm_l_para_sd}},
#' \code{\link{lsm_l_para_cv}}
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
        class = as.integer(para_sd$class),
        id = as.integer(NA),
        metric = "perimeter-area-ratio (sd)",
        value = as.double(para_sd$value)
    )
}
