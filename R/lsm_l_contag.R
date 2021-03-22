#' CONTAG (landscape level)
#'
#' @description Contagion (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @details
#' \deqn{CONTAG = 1 + \frac{\sum \limits_{q = 1}^{n_{a}} p_{q} ln(p_{q})}{2ln(t)}}
#'
#' where \eqn{p_{q}} the adjacency table for all classes divided by the sum of that table and
#' \eqn{t} the number of classes in the landscape.
#'
#' CONTAG is an 'Aggregation metric'. It is based on cell adjacencies and describes
#' the probability of two random cells belonging to the same class. \eqn{p_{q}} is
#' the cell adjacency table, where the order is preserved and pairs of adjacent cells
#' are counted twice. Contagion is affected by both the dispersion and interspersion
#' of classes. E.g., low class dispersion (= high proportion of like adjacencies) and
#' low interspersion (= uneven distribution of pairwise adjacencies) lead to a high
#' contagion value.
#'
#' The number of classes to calculate CONTAG must be >= than 2.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{0 < Contag <=100}
#' \subsection{Behaviour}{Approaches CONTAG = 0 if all cells are unevenly distributed
#' and 100 indicates that all cells are equally adjacent to all other classes.}
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
#' Riitters, K.H., O’Neill, R.V., Wickham, J.D. & Jones, K.B. (1996). A note on
#' contagion indices for landscape analysis. Landscape ecology, 11, 197–202.
#'
#' @export
lsm_l_contag <- function(landscape, verbose = TRUE) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_contag_calc,
                     verbose = verbose)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_contag_calc <- function(landscape, verbose) {

    # convert to raster to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "contag",
                              value = as.double(NA)))
    }

    t <- length(get_unique_values_int(landscape, verbose = FALSE))

    if (t < 2) {
        if (verbose) {
            warning("Number of classes must be >= 2: CONTAG = NA.",
                    call. = FALSE)
        }

        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "contag",
                              value = as.double(NA)))
    }

    else {

        adjacencies <- rcpp_get_coocurrence_matrix(landscape,
                                                   as.matrix(4))

        esum <- sum(adjacencies / sum(adjacencies) *
                        log(adjacencies / sum(adjacencies)), na.rm = TRUE)

        emax <- 2 * log(t)

        contag <- (1 + esum / emax) * 100

        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "contag",
                              value = as.double(contag)))
    }
}
