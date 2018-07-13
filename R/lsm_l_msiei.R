#' MSIEI (landscape level)
#'
#' @description Modified Simpson's evenness index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' @export
lsm_l_msiei <- function(landscape) UseMethod("lsm_l_msiei")

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_msiei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_msiei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_msiei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msiei
#' @export
lsm_l_msiei.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_msiei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_msiei_calc <- function(landscape) {

    msidi <- lsm_l_msidi_calc(landscape)

    pr <- lsm_l_pr_calc(landscape)

    msiei <- msidi$value / log(pr$value)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "msiei",
        value = as.double(msiei)
    )
}
