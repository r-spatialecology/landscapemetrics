#' CAI (patch level)
#'
#' @description Core area index (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.

#' @details
#' \deqn{CAI = (\frac{a_{ij}^{core}} {a_{ij}}) * 100}
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
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link{lsm_p_area}}, \cr
#' \code{\link{lsm_c_cai_mn}},
#' \code{\link{lsm_c_cai_sd}},
#' \code{\link{lsm_c_cai_cv}},
#' \code{\link{lsm_c_cpland}}, \cr
#' \code{\link{lsm_l_cai_mn}},
#' \code{\link{lsm_l_cai_sd}},
#' \code{\link{lsm_l_cai_cv}}
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
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
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

    area_patch <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = value * 10000)

    core_patch <- lsm_p_core_calc(landscape)

    cai_patch <- dplyr::mutate(core_patch,
                               value = value * 10000 / area_patch$value * 100)

    tibble::tibble(
        level = "patch",
        class = as.integer(area_patch$class),
        id = as.integer(cai_patch$id),
        metric = "cai",
        value = as.double(cai_patch$value)
    )
}
