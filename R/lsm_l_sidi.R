#' SIDI (landscape level)
#'
#' @description Simpson's diversity index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' lsm_l_sidi(landscape)
#'
#' @aliases lsm_l_sidi
#' @rdname lsm_l_sidi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Simpson, E. H. 1949. Measurement of diversity. Nature 163:688
#'
#' @export
lsm_l_sidi <- function(landscape, directions) UseMethod("lsm_l_sidi")

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_sidi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_sidi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_sidi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_sidi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_sidi_calc <- function(landscape, directions) {

    sidi <- landscape %>%
        lsm_c_pland_calc(directions = directions) %>%
        dplyr::mutate(value = (value / 100) ^ 2) %>%
        dplyr::summarise(value = 1 - sum(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "sidi",
        value = as.double(sidi$value)
    )
}
