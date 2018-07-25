#' LSI (class level)
#'
#' @description Landscape shape index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{LSI = \frac{e_{i}} {\min e_{i}}}
#' where \eqn{e_{i}} is the total edge length in cell surfaces and \eqn{\min e_{i}}
#' is the minimum total edge length in cell surfaces
#'
#' LSI is an 'Aggregation metric'. It is the ratio between the actual edge length of
#' class i and the hypothetical minimum edge length of class i. The minimum edge length equals
#' the edge length if class i would be maximally aggregated.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{LSI >= 1}
#' \subsection{Behaviour}{Equals LSI = 1 when only one squared patch is present or all
#' patches are maximally aggregated. Increases, without limit, as the length of the
#' actual edges increases, i.e. the patches become less compact.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}}, \cr
#' \code{\link{lsm_l_lsi}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_lsi(landscape)
#'
#' @aliases lsm_c_lsi
#' @rdname lsm_c_lsi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_lsi <- function(landscape, directions) UseMethod("lsm_c_lsi")

#' @name lsm_c_lsi
#' @export
lsm_c_lsi.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_lsi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lsi
#' @export
lsm_c_lsi.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_lsi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lsi
#' @export
lsm_c_lsi.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_lsi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_lsi
#' @export
lsm_c_lsi.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_lsi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_lsi_calc <- function(landscape, directions) {

    edge_class <- lsm_c_te_calc(landscape,
                                count_boundary = TRUE, directions = directions)

    area_class <- landscape %>%
        lsm_c_ca_calc(directions = directions) %>%
        dplyr::mutate(value = value * 10000)

    lsi <- dplyr::mutate(area_class,
                         n = trunc(sqrt(value)),
                         m = value - n^ 2,
                         minp = dplyr::case_when(
                             m == 0 ~ n * 4,
                             n ^ 2 < value & value <= n * (1 + n) ~ 4 * n + 2,
                             value > n * (1 + n) ~ 4 * n + 4),
                         value = edge_class$value / minp)

    tibble::tibble(
        level = "class",
        class = as.integer(edge_class$class),
        id = as.integer(edge_class$id),
        metric = "lsi",
        value = as.double(lsi$value)
    )

}
