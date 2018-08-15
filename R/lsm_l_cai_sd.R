#' CAI_SD (landscape level)
#'
#' @description Standard deviation of core area index (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CAI_{SD} = sd(CAI[patch_{ij}]}
#' where \eqn{CAI[patch_{ij}]} is the core area index of each patch.
#'
#' CAI_SD is a 'Core area metric'. The metric summarises the landscape
#' as the standard deviation of the core area index of all patches in the landscape.
#' The core area index is the percentage of core area in relation to patch area.
#' A cell is defined as core area if the cell has no neighbour with a different
#' value than itself (rook's case). The metric describes the differences among all patches
#' in the landscape.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{CAI_SD >= 0}
#' \subsection{Behaviour}{Equals CAI_SD = 0 if the core area index is identical for
#' all patches. Increases, without limit, as the variation of core area
#' indices increases.}
#'
#' @seealso
#' \code{\link{lsm_p_cai}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_cai_mn}},
#' \code{\link{lsm_c_cai_sd}},
#' \code{\link{lsm_c_cai_cv}}, \cr
#' \code{\link{lsm_l_cai_mn}},
#' \code{\link{lsm_l_cai_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_cai_sd(landscape)
#'
#' @aliases lsm_l_cai_sd
#' @rdname lsm_l_cai_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_cai_sd <- function(landscape, directions) UseMethod("lsm_l_cai_sd")

#' @name lsm_l_cai_sd
#' @export
lsm_l_cai_sd.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_cai_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_cai_sd
#' @export
lsm_l_cai_sd.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_cai_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_cai_sd
#' @export
lsm_l_cai_sd.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_cai_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_cai_sd
#' @export
lsm_l_cai_sd.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_cai_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_cai_sd
#' @export
lsm_l_cai_sd.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_cai_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_cai_sd_calc <- function(landscape, directions){

    cai_sd <- landscape %>%
        lsm_p_cai(., directions = directions) %>%
        dplyr::summarise(value = stats::sd(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "cai_sd",
        value = as.double(cai_sd$value)
    )
}
