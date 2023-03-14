#' SIEI (landscape level)
#'
#' @description Simpson's evenness index (Diversity metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SIEI = \frac{1 - \sum \limits_{i = 1}^{m} P_{i}^{2}} {1 - \frac{1} {m}}}
#' where \eqn{P_{i}} is the proportion of class i and \eqn{m} is the
#' number of classes.
#'
#' SIEI is a 'Diversity metric'. The metric is widely used in biodiversity and ecology.
#' It is the ratio between the actual Simpson's diversity  index and the theoretical maximum
#' Simpson's diversity index.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{0 < SIEI <= 1}
#' \subsection{Behaviour}{Equals SIEI = 0 when only one patch is present and approaches
#' SIEI = 1 when the number of class types increases while the proportions are
#' equally distributed}
#'
#' @seealso
#' \code{\link{lsm_c_pland}},
#' \code{\link{lsm_l_pr}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_siei(terra::unwrap(landscape))
#'
#' @aliases lsm_l_siei
#' @rdname lsm_l_siei
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' Simpson, E. H. 1949. Measurement of diversity. Nature 163:688
#'
#' @export
lsm_l_siei <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_siei_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_siei_calc <- function(landscape, directions, resolution = NULL) {

    sidi <- lsm_l_sidi_calc(landscape,
                            directions = directions,
                            resolution = resolution)

    # all values NA
    if (all(is.na(sidi$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "siei",
                              value = as.double(NA)))
    }

    pr <- lsm_l_pr_calc(landscape)

    siei <- sidi$value / (1 - (1 / pr$value))

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "siei",
                          value = as.double(siei)))
}
