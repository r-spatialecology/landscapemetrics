#' SIEI (landscape level)
#'
#' @description Simpson's evenness index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' lsm_l_siei(landscape)
#'
#' @aliases lsm_l_siei
#' @rdname lsm_l_siei
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_siei <- function(landscape) UseMethod("lsm_l_siei")

#' @name lsm_l_siei
#' @export
lsm_l_siei.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_siei
#' @export
lsm_l_siei.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_siei
#' @export
lsm_l_siei.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_siei
#' @export
lsm_l_siei.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_siei_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_siei_calc <- function(landscape) {

    sidi <- lsm_l_sidi_calc(landscape)
    pr <- lsm_l_pr_calc(landscape)

    siei <- sidi$value / (1 - (1 / pr$value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "siei",
        value = as.double(siei)
    )
}
