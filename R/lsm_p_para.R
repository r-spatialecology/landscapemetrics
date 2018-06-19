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
#' is not scale independent.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA > 0}
#' \subsection{Behaviour}{PARA increases, without limit, as the shape comlexity increases.}
#'
#' @seealso \code{\link{lsm_p_area}} and \code{\link{lsm_p_perim}}
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

    perimeter <- lsm_p_perim_calc(landscape)

    area <- lsm_p_area_calc(landscape) %>%
        dplyr::mutate(value = value * 10000)

    para <- perimeter$value / area$value

    tibble::tibble(
        level = "patch",
        class = perimeter$class,
        id = perimeter$id,
        metric = "perimeter-area-ratio",
        value = para
    )
}
