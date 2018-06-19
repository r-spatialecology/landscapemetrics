#' CAI (patch level)
#'
#' @description Core area index (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.

#' @details
#' \deqn{CAI = (\frac{a_{ij}^{core}}{a_{ij}}) * 100}
#' where \eqn{a_{ij}^{core}} is the core area in square meters and \eqn{a_{ij}}
#' is the area in square meters.
#'
#' CAI is a 'Core area metric'. It equals the percentage of a patch that is core area.
#' A cell is defined as core area if the cell has no neighbour with a different value
#' than itself (rook's case). It describes patch area and shape simultaneously (more core area
#' when the patch is large and the shape is rather compact, i.e. a square). Because the index is
#' relative, it is comparable among patches with different area.
#'
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{0 <= CAI <= 100}
#' \subsection{Behaviour}{CAI = 0 when the patch has no core area and
#' approaches CAI = 100 with increasing percentage of core area within a patch.}
#'
#' @seealso \code{\link{lsm_p_core}} and code{\link{lsm_p_area}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_cai(landscape)
#'
#' @aliases lsm_p_cai
#' @rdname lsm_p_cai
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_cai <- function(landscape) UseMethod("lsm_p_cai")

#' @name lsm_p_cai
#' @export
lsm_p_cai.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_cai_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_cai
#' @export
lsm_p_cai.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_cai_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_cai
#' @export
lsm_p_cai.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_cai_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_cai
#' @export
lsm_p_cai.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_cai_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_cai_calc <- function(landscape){

    area <- landscape%>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = value * 10000)

    cai <- landscape %>%
        lsm_p_core_calc() %>%
        dplyr::mutate(value = value * 10000 / area$value * 100)

    tibble::tibble(
        level = "patch",
        class = cai$class,
        id = cai$id,
        metric = "core area index",
        value = cai$value
    )
}
