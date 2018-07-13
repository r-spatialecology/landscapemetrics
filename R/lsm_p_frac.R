#' FRAC (patch level)
#'
#' @description Fractal dimension index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{FRAC = \frac{2 * \ln * (0.25 * p_{ij})} {\ln a_{ij}}}
#' where \eqn{p_{ij}} is the perimeter in meters and \eqn{a_{ij}} is the
#' area in square meters
#'
#' FRAC is a 'Shape metric'. The index is based on the patch perimeter and
#' the patch area and describes the patch complexity. Because it is standardized,
#' it is scale independent, meaning that increasing the patch size while not changing the
#' patch form will not change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= FRAC <= 2 }
#' \subsection{Behaviour}{Approaches FRAC = 1 for a squared patch shape form and FRAC = 2
#' for a irregular patch shape.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_p_perim}}, \cr
#' \code{\link{lsm_c_frac_mn}},
#' \code{\link{lsm_c_frac_sd}},
#' \code{\link{lsm_c_frac_cv}}, \cr
#' \code{\link{lsm_l_frac_mn}},
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_frac(landscape)
#'
#' @aliases lsm_p_frac
#' @rdname lsm_p_frac
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_frac <- function(landscape) UseMethod("lsm_p_frac")

#' @name lsm_p_frac
#' @export
lsm_p_frac.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_frac
#' @export
lsm_p_frac.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_frac
#' @export
lsm_p_frac.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_frac
#' @export
lsm_p_frac.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_frac_calc <- function(landscape){

    perimeter_patch <- lsm_p_perim_calc(landscape)

    area_patch <- lsm_p_area_calc(landscape)

    frac_patch <- dplyr::mutate(area_patch,
                                value = 2 * log (0.25 * perimeter_patch$value) /
                                    log(value * 10000))

    frac_patch[is.na(frac_patch)] <- 1

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_patch$class),
        id = as.integer(perimeter_patch$id),
        metric = "frac",
        value = as.double(frac_patch$value)
    )
}
