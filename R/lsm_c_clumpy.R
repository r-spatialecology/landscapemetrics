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

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_clumpy_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_clumpy
#' @export
lsm_c_clumpy.RasterStack <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_clumpy_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_clumpy
#' @export
lsm_c_clumpy.RasterBrick <- function(landscape) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_clumpy_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_clumpy
#' @export
lsm_c_clumpy.stars <- function(landscape) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_clumpy_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}


#' @name lsm_c_clumpy
#' @export
lsm_c_clumpy.list <- function(landscape) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_clumpy_calc)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_c_clumpy_calc <- function(landscape, resolution = NULL){

    # pad landscape to also include adjacencies at landscape boundary
    landscape_padded <- pad_raster(landscape)

    # get coocurrence
    tb <- rcpp_get_coocurrence_matrix(landscape_padded,
                                      directions = as.matrix(4))

    # like adacencies are on the diagonal and remove adjacencies to boundary
    like_adjacencies <- diag(tb)[2:length(diag(tb))]

    # all other adjacencies
    other_adjacencies <- as.matrix(tb[, 2:ncol(tb)])

    # number cells in each class without -999 lansdcape boundary
    cells_class <- rcpp_get_composition_vector(landscape_padded)[-1]

    # conver to tibble
    cells_class <- tibble::tibble(class = names(cells_class),
                                  value = cells_class)

    # calculate minimum perimeter
    min_e <- dplyr::mutate(cells_class,
                                       n = trunc(sqrt(value)),
                                       m = value - n ^ 2,
                                       min_e = dplyr::case_when(
                                           m == 0 ~ n * 4,
                                           n ^ 2 < value & value <= n * (1 + n) ~ 4 * n + 2,
                                           value > n * (1 + n) ~ 4 * n + 4)
                           )

    # calculate g_i
    g_i <- like_adjacencies / (colSums(other_adjacencies) - min_e$min_e)

    # proportional class area - direction has no influence on PLAND
    prop_class <- lsm_c_pland_calc(landscape,
                                   directions = 8,
                                   resolution = resolution)

    prop_class <- prop_class$value / 100

    # calculate clumpy
    clumpy <- sapply(seq_along(g_i), function(row_ind) {

        # set to NA if mathematical not possible
        if (is.nan(g_i[row_ind]) || is.na(g_i[row_ind]) || prop_class[row_ind] == 1) {
            clumpy <- NA
        }

        else if (g_i[row_ind] < (prop_class[row_ind]) & prop_class[row_ind] < .5) {
            clumpy <- (g_i[row_ind] - prop_class[row_ind]) / prop_class[row_ind]
        }

        else {
            clumpy <- (g_i[row_ind] - prop_class[row_ind]) / (1 - prop_class[row_ind])
        }
    })

    tibble::tibble(
        level = "class",
        class = as.integer(names(clumpy)),
        id = as.integer(NA),
        metric = "clumpy",
        value = as.double(clumpy)
    )
}
