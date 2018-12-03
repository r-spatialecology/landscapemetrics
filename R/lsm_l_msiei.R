#' MSIEI (landscape level)
#'
#' @description Modified Simpson's evenness index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' lsm_l_msiei(landscape)
#'
#' @aliases lsm_l_msiei
#' @rdname lsm_l_msiei
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Simpson, E. H. 1949. Measurement of diversity. Nature 163:688
#'
#' Pielou, E. C. 1975. Ecological Diversity. Wiley-Interscience, New York.
#'
#' @export
lsm_l_msiei <- function(landscape, directions) UseMethod("lsm_l_msiei")

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_msiei_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_msiei_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_msiei_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_msiei_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_msiei_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_l_msiei_calc <- function(landscape, directions, resolution = NULL) {

    msidi <- lsm_p_area_calc(landscape,
                             directions = directions,
                             resolution = resolution)

    msidi <- dplyr::summarise(dplyr::group_by(msidi, class),
                              value = sum(value))

    msidi <- dplyr::summarise(dplyr::mutate(msidi, value = (value / sum(value)) ^ 2),
                              value = -log(sum(value)))

    pr <- length(get_unique_values(landscape)[[1]])

    msiei <- msidi$value / log(pr)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "msiei",
        value = as.double(msiei)
    )
}
