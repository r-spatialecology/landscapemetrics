#' CIRCLE_SD (landscape level)
#'
#' @description Standard deviation of related circumscribing circle (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CIRCLE_{SD} = sd(CIRCLE[patch_{ij}])}
#' where \eqn{CIRCLE[patch_{ij}]} is the related circumscribing circle of each patch.
#'
#' CIRCLE_SD is a 'Shape metric' and summarises the landscape as the standard deviation of
#' the related circumscribing circle of all patches in the landscape. CIRCLE describes
#' the ratio between the patch area and the smallest circumscribing circle of the patch
#' and characterises the compactness of the patch. The metric describes the differences
#' among all patches of the landscape.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CIRCLE_SD >= 0}
#' \subsection{Behaviour}{Equals CIRCLE_SD if the related circumscribing circle is identical
#' for all patches. Increases, without limit, as the variation of related circumscribing
#' circles increases.}
#'
#' @seealso
#' \code{\link{lsm_p_circle}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_circle_mn}},
#' \code{\link{lsm_c_circle_sd}},
#' \code{\link{lsm_c_circle_cv}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_circle_sd(landscape)
#'
#' @aliases lsm_l_circle_sd
#' @rdname lsm_l_circle_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_circle_sd <- function(landscape, directions) UseMethod("lsm_l_circle_sd")

#' @name lsm_l_circle_sd
#' @export
lsm_l_circle_sd.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_circle_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_circle_sd
#' @export
lsm_l_circle_sd.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_circle_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_sd
#' @export
lsm_l_circle_sd.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_circle_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_circle_sd
#' @export
lsm_l_circle_sd.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_circle_sd_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_circle_sd_calc <- function(landscape, directions) {

    circle_mn <- landscape %>%
        lsm_p_circle_calc(directions = directions) %>%
        dplyr::summarize(value = stats::sd(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "circle_sd",
        value = as.double(circle_mn$value)
    )

}

