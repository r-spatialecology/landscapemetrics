#' AI (landscape level)
#'
#' @description Aggregation index (Contagion/Interspersion metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers
#' @param directions The number of directions in which cells should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AI = \Bigg[\sum\limits_{i=1}^m \Big( \frac{g_{ii}}{max-g_{ii}} \Big) P_{i} \Bigg](100) }
#'
#' where \eqn{g_{ii}} is the number of like adjacencies based on the single-count method and
#' \eqn{max-g_{ii}} is the classwise maximum number of like adjacencies of class i and \eqn{P_{i}}
#' the proportion of landscape compromised of class i.
#'
#' CLUMPY is an 'Contagion/Interspersion metric'. It equals the number of like adjacencies divided
#' by the theoretical maximum possible number of like adjacencies for that class summed over each
#' class for the entire landscape.
#' The metric is based on he adjacency matrix and the the single-count method.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{0 <= AI <= 100}
#' \subsection{Behaviour}{Equals 0 for maximally disaggregated and 100
#'  for maximally aggregated classes.}
#'
#' @return tibble
#' @seealso
#' \code{\link{lsm_c_ai}}
#'
#' @examples
#' lsm_l_ai(landscape)
#'
#' @aliases lsm_l_ai
#' @rdname lsm_l_ai
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_ai <- function(landscape, directions) UseMethod("lsm_l_ai")

#' @name lsm_l_ai
#' @export
lsm_l_ai.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_ai_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ai
#' @export
lsm_l_ai.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_ai_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ai
#' @export
lsm_l_ai.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_ai_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_ai
#' @export
lsm_l_ai.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_ai_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_ai_calc <- function(landscape, directions) {

    cai <- lsm_c_ai(landscape, directions = directions) %>%
        dplyr::pull(value)

    prop_class <- (lsm_c_pland(landscape) %>%
                       dplyr::pull(value)) / 100

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "ai",
        value = as.double(sum(cai * prop_class))
    )
}




