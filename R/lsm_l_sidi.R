#' SIDI (landscape level)
#'
#' @description Simpson's diversity index (Diversity metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{SIDI = 1 - \sum \limits_{i = 1}^{m} P_{i}^{2}}
#' where \eqn{P_{i}} is the proportion of class i and \eqn{m} is the
#' number of classes.
#'
#' SIDI is a 'Diversity metric'. It is widely used in biodiversity and ecology. It is
#' less sensitive to rare class types than \code{\link{lsm_l_shdi}}. It can be interpreted
#' as the probability that two randomly selected cells belong to the same class.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{0 <= SIDI < 1}
#' \subsection{Behaviour}{Equals SIDI = 0 when only one patch is present and approaches
#' SIDI < 1 when the number of class types increases while the proportions are equally
#' distributed}
#'
#' @seealso
#' \code{\link{lsm_c_pland}},
#' \code{\link{lsm_l_pr}}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_l_sidi(landscape)
#'
#' @aliases lsm_l_sidi
#' @rdname lsm_l_sidi
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
lsm_l_sidi <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_l_sidi_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_l_sidi_calc <- function(landscape, directions, resolution = NULL) {

    sidi <- lsm_c_pland_calc(landscape,
                             directions = directions,
                             resolution = resolution)

    # all values NA
    if (all(is.na(sidi$value))) {
        return(tibble::tibble(level = "landscape",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "sidi",
                              value = as.double(NA)))
    }

    sidi <- 1 - sum((sidi$value / 100) ^ 2)

    return(tibble::tibble(level = "landscape",
                          class = as.integer(NA),
                          id = as.integer(NA),
                          metric = "sidi",
                          value = as.double(sidi)))
}
