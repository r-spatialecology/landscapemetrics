#' CLUMPY (class level)
#'
#' @description Clumpiness index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers
#'
#' @details
#' \deqn{Given G_{i} = \Bigg(\frac{g_{ii}}{ (\sum\limits_{k=1}^m g_{ik}) - min e_{i}} \Bigg)}
#' \deqn{CLUMPY = \Bigg[ \frac{G_{i} - P_{i}}{P_{i}} for G_{i} < P_{i} \& P_{i} < .5; else \\  \frac{G_{i} - P_{i}}{1 -P_{i}} \Bigg] }
#'
#' where \eqn{g_{ii}} is the number of like adjacencies, \eqn{g_{ik}} is the classwise
#' number of all adjacencies including the focal class, \eqn{min e_{i}} is the
#' minimum perimeter of the total class in terms of cell surfaces assuming total clumping and
#' \eqn{P_{i}} is the proportion of landscape occupied by each class.
#'
#' CLUMPY is an 'Aggregation metric'. It equals the proportional deviation of
#' the proportion of like adjacencies involving the corresponding class from that expected
#' under a spatially random distribution. The metric is based on he adjacency matrix and the
#' the double-count method.
#'
#' \subsection{Units}{None}, directions = directions
#' \subsection{Range}{-1 <= CLUMPY <= 1}
#' \subsection{Behaviour}{Equals -1 for maximally disaggregated, 0 for randomly distributed
#' and 1 for maximally aggregated classes.}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_clumpy(landscape)
#'
#' @aliases lsm_c_clumpy
#' @rdname lsm_c_clumpy
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_clumpy <- function(landscape) UseMethod("lsm_c_clumpy")

#' @name lsm_c_clumpy
#' @export
lsm_c_clumpy.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_clumpy_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_clumpy
#' @export
lsm_c_clumpy.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_clumpy_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_clumpy
#' @export
lsm_c_clumpy.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_clumpy_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_clumpy
#' @export
lsm_c_clumpy.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_clumpy_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_clumpy_calc <- function(landscape){

    landscape_padded <- pad_raster(landscape)

    tb <- rcpp_get_coocurrence_matrix(raster::as.matrix(landscape_padded),
                                      directions = as.matrix(4))

    like_adjacencies <- diag(tb)
    like_adjacencies <- like_adjacencies[2:length(like_adjacencies)]
    other_adjacencies <- as.matrix(tb[, 2:ncol(tb)])

    area_class <- tibble::as.tibble(raster::freq(landscape, useNA = "no"))

    min_e <- dplyr::mutate(
        area_class,
        value = count * 10000,
        n = trunc(sqrt(count)),
        m = count - n ^ 2,
        min_e = dplyr::case_when(
            m == 0 ~ n * 4,
            n ^ 2 < count &
                count <= n * (1 + n) ~ 4 * n + 2,
            count > n * (1 + n) ~ 4 * n + 4
        )
    ) %>%
        dplyr::pull(min_e)

    g <- like_adjacencies / (colSums(other_adjacencies) - min_e)

    prop_class <-
        (lsm_c_pland(landscape) %>% dplyr::pull(value)) / 100

    clumpy <- purrr::map_dbl(seq_along(g), function(row_ind) {

        if (is.nan(g[row_ind]) ||
            is.na(g[row_ind]) ||
            prop_class[row_ind] == 1) {
            clumpy <- NA
        }

        else if (g[row_ind] < (prop_class[row_ind]) & prop_class[row_ind] < .5) {
            clumpy <-
                (g[row_ind] - prop_class[row_ind]) / prop_class[row_ind]
        }

        else {
            clumpy <-
                (g[row_ind] - prop_class[row_ind]) / (1 - prop_class[row_ind])
        }
    })

    tibble::tibble(
        level = "class",
        class = as.integer(names(g)),
        id = as.integer(NA),
        metric = "clumpy",
        value = as.double(clumpy)
    )

}
