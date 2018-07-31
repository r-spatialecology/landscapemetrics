#' MSIDI (landscape level)
#'
#' @description Modified Simpson's diversity index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{MSIDI = -\ln \sum \limits_{i = 1}^{m} P_{i}^{2}}
#' where \eqn{P_{i}} is the landscape area proportion of class i.
#'
#' MSIDI is a 'Diversity metric'.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{MSIDI >= 0}
#' \subsection{Behaviour}{MSIDI = 0 if only one patch is present and increases, without
#' limit, as the amount of patches with equally distributed landscape proportions increases}
#'
#' @seealso
#' \code{\link{lsm_l_sidi}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_msidi(landscape)
#'
#' @aliases lsm_l_msidi
#' @rdname lsm_l_msidi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_msidi <- function(landscape, directions) UseMethod("lsm_l_msidi")

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_msidi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_msidi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_msidi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_msidi
#' @export
lsm_l_msidi.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_msidi_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_msidi_calc <- function(landscape, directions) {

    msidi <- landscape %>%
        lsm_p_area_calc(directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(value = (value / sum(value, na.rm = TRUE)) ^ 2) %>%
        dplyr::summarise(value = -log(sum(value, na.rm = TRUE)))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "msidi",
        value = as.double(msidi$value)
    )
}
