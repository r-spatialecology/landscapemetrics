#' CLUMPY (class level)
#'
#' @description Clumpiness index (Aggregation metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters
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
#' \subsection{Units}{None}
#' \subsection{Range}{-1 <= CLUMPY <= 1}
#' \subsection{Behaviour}{Equals -1 for maximally disaggregated, 0 for randomly distributed
#' and 1 for maximally aggregated classes.}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_c_clumpy(landscape)
#'
#' @aliases lsm_c_clumpy
#' @rdname lsm_c_clumpy
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_c_clumpy <- function(landscape) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_c_clumpy_calc)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_c_clumpy_calc <- function(landscape, resolution = NULL){

    # pad landscape to also include adjacencies at landscape boundary
    landscape_padded <- pad_raster_internal(landscape,
                                            pad_raster_value = -999,
                                            pad_raster_cells = 1,
                                            global = FALSE)

    landscape_padded[!is.finite(landscape_padded)] <- -999

    # all values NA
    if (all(landscape_padded %in% c(NA, -999))) {
        return(tibble::tibble(level = "class",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "clumpy",
                              value = as.double(NA)))
    }

    # get coocurrence
    tb <- rcpp_get_coocurrence_matrix(landscape_padded,
                                      directions = as.matrix(4))

    # like adacencies are on the diagonal and remove adjacencies to boundary
    like_adjacencies <- diag(tb)[2:length(diag(tb))]

    # all other adjacencies
    other_adjacencies <- as.matrix(tb[, 2:ncol(tb)])

    # number cells in each class without -999 lansdcape boundary
    cells_class <- rcpp_get_composition_vector(landscape_padded)[-1]

    # convert to tibble
    cells_class <- tibble::tibble(class = names(cells_class),
                                  value = cells_class)

    # calculate minimum perimeter
    cells_class$n <- trunc(sqrt(cells_class$value))
    cells_class$m <- cells_class$value - cells_class$n ^ 2
    cells_class$min_e <- ifelse(test = cells_class$m == 0,
                                yes = cells_class$n * 4,
                                no = ifelse(test = cells_class$n ^ 2 < cells_class$value & cells_class$value <= cells_class$n * (1 + cells_class$n),
                                            yes = 4 * cells_class$n + 2,
                                            no = ifelse(test = cells_class$value > cells_class$n * (1 + cells_class$n),
                                                        yes = 4 * cells_class$n + 4,
                                                        no = NA)))

    # test if any NAs introduced
    if (anyNA(cells_class$min_e)) {
        warning("NAs introduced by lsm_c_clumpy", call. = FALSE)
    }

    # calculate g_i
    g_i <- like_adjacencies / (colSums(other_adjacencies) - cells_class$min_e)

    # proportional class area - direction has no influence on PLAND
    prop_class <- lsm_c_pland_calc(landscape,
                                   directions = 8,
                                   resolution = resolution)

    prop_class <- prop_class$value / 100

    # calculate clumpy
    clumpy <- vapply(seq_along(g_i), FUN = function(row_ind) {

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
    }, FUN.VALUE = numeric(1))

    return(tibble::tibble(level = "class",
                          class = as.integer(names(g_i)),
                          id = as.integer(NA),
                          metric = "clumpy",
                          value = as.double(clumpy)))
}
