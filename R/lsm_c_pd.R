#' PD (class level)
#'
#' @description Patch density (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{PD = \frac{n_{i}} {A} * 10000 * 100}
#' where \eqn{n_{i}} is the number of patches and \eqn{A} is the total landscape
#' area in square meters.
#'
#' PD is an 'Aggregation metric'. It describes the fragmentation of a class, however, does not
#' necessarily contain information about the configuration or composition of the class. In
#' contrast to \code{\link{lsm_c_np}} it is standardised to the area and comparisons among
#' landscapes with different total area are possible.
#'
#' \subsection{Units}{Number per 100 hectares}
#' \subsection{Ranges}{0 < PD <= 1e+06}
#' \subsection{Behaviour}{Increases as the landscape gets more patchy. Reaches its maximum
#' if every cell is a different patch.}
#'
#' @seealso
#' \code{\link{lsm_c_np}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_pd}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_pd(landscape)
#'
#' @aliases lsm_c_pd
#' @rdname lsm_c_pd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_pd <- function(landscape) UseMethod("lsm_c_pd")

#' @name lsm_c_pd
#' @export
lsm_c_pd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pd
#' @export
lsm_c_pd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pd
#' @export
lsm_c_pd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pd
#' @export
lsm_c_pd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_pd_calc <- function(landscape) {

    area_landscape <- lsm_l_ta_calc(landscape)

    np_class <- lsm_c_np_calc(landscape)

    patch_density <- dplyr::mutate(np_class, value = (value / area_landscape$value) * 100)

    tibble::tibble(
        level = "class",
        class = as.integer(patch_density$class),
        id = as.integer(NA),
        metric = "patch density",
        value = as.double(patch_density$value)
    )
}
