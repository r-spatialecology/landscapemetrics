#' CONTAG (landscape level)
#'
#' @description Contagion (Contagion and Interspersion metrics)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CONTAG = 1 + \fract{\sum \limits_{q = 1}^{n_{a}} p_{q} ln(p_{q})}{2ln(t)}}
#'
#' where \eqn{p_{q}} the adjacency table for all classes divided by the sum of that table.
#'
#' CONTAG is an 'Aggregation metric'. It is based on cell adjacencies and describes
#' the probability of two random cells belonging to the same class. \eqn{p_{q}} is
#' the cell adjacency table, where the order is preserved and pairs of adjacent cells
#' are counted twice. Contagion is affected by both the dispersion and interspersion
#' of classes. E.g., low class dispersion (= high proportion of like adjacencies) and
#' low interspersion (= uneven distribution of pairwise adjacencies) lead to a high
#' contagion value.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{0 < Contag <=100}
#' \subsection{Behaviour}{Approaches CONTAG = 0 if all cells are unevenly distributed
#' and 100 indicates that all cells are equally adjacent to all other classes.}
#'
#' @seealso
#' \code{\link{lsm_p_perim}}
#' \code{\link{lsm_l_contag}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_contag(landscape)
#'
#' @aliases lsm_l_contag
#' @rdname lsm_l_contag
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_contag <- function(landscape) UseMethod("lsm_l_contag")

#' @name lsm_l_contag
#' @export
lsm_l_contag.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_contag_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_contag
#' @export
lsm_l_contag.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_contag_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_contag
#' @export
lsm_l_contag.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_contag_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}


#' @name lsm_l_contag
#' @export
lsm_l_contag.list <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_contag_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_contag_calc <- function(landscape) {

    t <- lsm_l_pr(landscape)$value

    adjacencies <-
        rcpp_get_coocurrence_matrix(raster::as.matrix(landscape),
                                    as.matrix(4))

    esum <- sum(adjacencies / sum(adjacencies) *
                    log(adjacencies / sum(adjacencies)),
                na.rm = TRUE)

    emax <- 2 * log(t)


    contag <- (1 + esum / emax) * 100

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "contag",
        value = as.double(contag)
    )
}
