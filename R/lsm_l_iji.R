#'Interspersion and Juxtaposition index (landscape level)
#
#' @description Interspersion and Juxtaposition index (Contagion and Interspersion metrics)
#
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#
#' @details
#' ???
#' \deqn{???}
#' \subsection{Units}{???}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#
#' @return tibble
#
#' @examples
#'lsm_l_iji(landscape)
#
#' @aliases lsm_l_iji
#' @rdname lsm_l_iji
#
#' @references
#'McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#'Program for Categorical and Continuous Maps. Computer software program produced by
#'the authors at the University of Massachusetts, Amherst. Available at the following
#'web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#
#' @export
lsm_l_iji <- function(landscape) UseMethod("lsm_l_iji")
#' @name lsm_l_iji
#' @export
lsm_l_iji.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_iji_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}
#' @name lsm_l_iji
#' @export
lsm_l_iji.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_iji_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}
#' @name lsm_l_iji
#' @export
lsm_l_iji.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_iji_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}
#' @name lsm_l_iji
#' @export
lsm_l_iji.list <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_iji_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_iji_calc <- function(landscape) {

    adjacencies <- rcpp_get_coocurrence_matrix(raster::as.matrix(landscape),
                                               as.matrix(4))

    diag(adjacencies) <- 0

    e_total <- sum(adjacencies[lower.tri(adjacencies)])

    edge_ratio <- (adjacencies / e_total) * log(adjacencies / e_total)
    edge_ratio <- edge_ratio[lower.tri(edge_ratio)]

    landscape_sum <- -sum(edge_ratio, na.rm = TRUE)

    iji <-
        (landscape_sum /
             log(0.5  * (ncol(adjacencies) * (ncol(adjacencies)  - 1)))) * 100

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "iji",
        value = as.double(iji)
    )
}
