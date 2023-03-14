#' MSIEI (landscape level)
#'
#' @description Modified Simpson's evenness index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{MSIEi = \frac{- \ln \sum \limits_{i = 1}^{m} P_{i}^{2}} {\ln m}}
#' where \eqn{P_{i}} is the landscape area proportion of class i.

#' MSIEI is a 'Diversity metric'.

#' \subsection{Units}{None}
#' \subsection{Range}{0 <= MSIEI < 1}
#' \subsection{Behaviour}{MSIEI = 0 when only one patch is present and approaches
#' MSIEI = 1 as the proportional distribution of patches becomes more even}
#'
#' @seealso
#' \code{\link{lsm_l_siei}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_msiei(terra::unwrap(landscape))
#'
#' @aliases lsm_l_msiei
#' @rdname lsm_l_msiei
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' Simpson, E. H. 1949. Measurement of diversity. Nature 163:688
#'
#' Pielou, E. C. 1975. Ecological Diversity. Wiley-Interscience, New York.
#'
#' @export
lsm_l_msiei <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_msiei_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_msiei_calc <- function(landscape, directions, resolution = NULL) {

    patch_area <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # all values NA
    if (all(is.na(patch_area$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "msiei",
                              value = as.double(NA)))
    }

    msidi <- stats::aggregate(x = patch_area[, 5], by = patch_area[, 2],
                              FUN = sum)

    msidi <- -log(sum((msidi$value / sum(msidi$value)) ^ 2))

    pr <- length(get_unique_values_int(landscape, verbose = FALSE))

    msiei <- msidi / log(pr)

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "msiei",
                          value = as.double(msiei)))
}
