#' PARA (patch level)
#'
#' @description Perimeter-Area ratio (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{PARA = \frac{p_{ij}}{a_{ij}}}
#' where \eqn{p_{ij}} is the perimeter in meters and \eqn{a_{ij}} is the
#' area in square meters.
#'
#' PARA is a 'Shape metric'. It describes the patch complexity in a straightforward way.
#' However, because it is not standarised to a certain shape (e.g. a square), it
#' is not scale independent, meaning that increasing the patch size while not changing the
#' patch form will change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA > 0}
#' \subsection{Behaviour}{Increases, without limit, as the shape comlexity increases.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_p_perim}}, \cr
#' \code{\link{lsm_c_para_mn}},
#' \code{\link{lsm_c_para_sd}},
#' \code{\link{lsm_c_para_cv}}, \cr
#' \code{\link{lsm_l_para_mn}},
#' \code{\link{lsm_l_para_sd}},
#' \code{\link{lsm_l_para_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_para(landscape)
#'
#' @aliases lsm_p_para
#' @rdname lsm_p_para
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_para <- function(landscape) UseMethod("lsm_p_para")

#' @name lsm_p_para
#' @export
lsm_p_para.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_para_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_para
#' @export
lsm_p_para.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_para_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_para
#' @export
lsm_p_para.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_para_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_para
#' @export
lsm_p_para.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_para_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_para_calc <- function(landscape){

    perimeter_patch <- lsm_p_perim_calc(landscape)

    para_patch <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = perimeter_patch$value / (value * 10000))

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_patch$class),
        id = as.integer(perimeter_patch$id),
        metric = "perimeter-area-ratio",
        value = as.double(para_patch$value)
    )
}
