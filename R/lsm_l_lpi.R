#' LPI (landscape level)
#'
#' @description Largest patch index (Area and Edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{LPI = \frac{\max (a_{ij})} {A} * 100}
#' where \eqn{max(a_{ij})} is the area of the patch in square meters and \eqn{A}
#' is the total landscape area in square meters.
#'
#' The largest patch index is an 'Area and edge metric'. It is the percentage of the
#' landscape covered by the largest patch in the landscape. It is a simple
#' measure of dominance.
#'
#' \subsection{Units}{Percentage}
#' \subsection{Range}{0 < LPI <= 100}
#' \subsection{Behaviour}{Approaches LPI = 0 when the largest patch is becoming small
#' and equals LPI = 100 when only one patch is present}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_c_lpi}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_lpi(landscape)
#'
#' @aliases lsm_l_lpi
#' @rdname lsm_l_lpi
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_lpi <- function(landscape) UseMethod("lsm_l_lpi")

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_lpi
#' @export
lsm_l_lpi.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_lpi_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_lpi_calc <- function(landscape) {

    area_landscape <- lsm_l_ta_calc(landscape)

    area_patch <- lsm_p_area_calc(landscape)

    lpi <- area_patch %>%
        dplyr::mutate(lpi = (value / area_landscape$value) * 100) %>%
        dplyr::summarise(value = max(lpi, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "lpi",
        value = as.double(lpi$value)
    )
}
