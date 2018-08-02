#' nLSI (class level)
#'
#' @description Normalized landscape shape index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{nlsi = \frac{e_{i}} {\min e_{i}}}
#' where \eqn{e_{i}} is the total edge length in cell surfaces and \eqn{\min e_{i}}
#' is the minimum total edge length in cell surfaces
#'
#' nlsi is an 'Aggregation metric'. It is the ratio between the actual edge length of
#' class i and the hypothetical minimum edge length of class i. The minimum edge length equals
#' the edge length if class i would be maximally aggregated.
#'
#' \subsection{Units}{None}
#' \subsection{Ranges}{nlsi >= 1}
#' \subsection{Behaviour}{Equals nlsi = 1 when only one squared patch is present or all
#' patches are maximally aggregated. Increases, without limit, as the length of the
#' actual edges increases, i.e. the patches become less compact.}
#'
#' @seealso
#' \code{\link{lsm_p_shape}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_nlsi(landscape)
#'
#' @aliases lsm_c_nlsi
#' @rdname lsm_c_nlsi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Patton, D. R. 1975. A diversity index for quantifying habitat "edge".
#' Wildl. Soc.Bull. 3:171-173.
#'
#' @export
lsm_c_nlsi <- function(landscape, directions) UseMethod("lsm_c_nlsi")

#' @name lsm_c_nlsi
#' @export
lsm_c_nlsi.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_nlsi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_nlsi
#' @export
lsm_c_nlsi.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_nlsi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_nlsi
#' @export
lsm_c_nlsi.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_nlsi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_nlsi
#' @export
lsm_c_nlsi.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_nlsi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_nlsi_calc <- function(landscape, directions) {

    edge_class <- lsm_c_te_calc(landscape,
                                count_boundary = T, directions = directions)

    ai <- rcpp_get_composition_vector(raster::as.matrix(landscape))

    pi <- prop.table(ai)

    A <- sum(ai)
    B <- (raster::ncol(landscape) * 2) + (raster::nrow(landscape) * 2)
    Z <- lsm_l_te_calc(landscape, count_boundary = TRUE) %>% dplyr::pull(value)

    nlsi <- tibble::tibble(ai = ai,
                           pi = pi,
                           A  = A,
                           B  = B,
                           Z  = Z)

    min_e <- dplyr::mutate(nlsi,
                         n = trunc(sqrt(ai)),
                         m = ai - n ^ 2,
                         min_e = dplyr::case_when(m == 0 ~ n * 4,
                                                  n ^ 2 < ai & ai <= n * (1 + n) ~ 4 * n + 2,
                                                  ai > n * (1 + n) ~ 4 * n + 4))


    max_e <- dplyr::mutate(nlsi,
                           max_e = dplyr::case_when(pi <= 0.5 ~ 4 * ai,
                                                    A %% 2 == 0 & pi > .5 & pi <= (.5 * A + .5 * B)/A ~ 3 * A - 2 * ai,
                                                    A %% 2 != 0 & pi > .5 & pi <= (.5 * A + .5 * B)/A ~ 3 * A - 2 * ai + 3,
                                                    pi >= (.5 * A + .5 * B)/A ~ Z + 4 * (A - ai)
                           )
    )

    result <- (edge_class$value - min_e$min_e) / (max_e$max_e - min_e$min_e)
    result[is.nan(result)] <- NA

    tibble::tibble(
        level = "class",
        class = as.integer(edge_class$class),
        id = as.integer(NA),
        metric = "nlsi",
        value = as.double(result)
    )

}
