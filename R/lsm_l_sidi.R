#' SIDI (landscape level)
#'
#' @description Simpson's diversity index (Diversity metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_sidi <- function(landscape) UseMethod("lsm_l_sidi")

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_sidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_sidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_sidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_sidi
#' @export
lsm_l_sidi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_sidi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_sidi_calc <- function(landscape) {

    sidi <- landscape %>%
        lsm_c_pland_calc() %>%
        dplyr::mutate(value = (value / 100) ^ 2) %>%
        dplyr::summarise(value = 1 - sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "sidi",
        value = as.double(sidi$value)
    )
}
