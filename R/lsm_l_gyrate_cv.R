#' GYRATE_CV (landscape level)
#'
#' @description Coeffiecent of variation radius of gyration (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{GYRATE_{CV} = cv(GYRATE[patch_{ij}])}
#' where \eqn{GYRATE[patch_{ij}]} equals the radius of gyration of each patch.
#'
#' GYRATE_CV is an 'Area and edge metric'. The metric summarises the landscape
#' as the coeffiecent of variation of the radius of gyration of all patches
#' in the landscape. GYRATE measures the distance from each cell to the patch
#' centroid and is based on cell center-to-cell center distances. The metrics characterises
#' both the patch area and compactness. The coeffiecent of variation is
#' scaled to the mean and comparable among different landscapes.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{GYRATE_CV >= 0 }
#' \subsection{Behaviour}{Equals GYRATE_CV = 0 if the radius of gyration is identical
#' for all patches. Increases, without limit, as the variation of the radius of gyration
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_gyrate}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_gyrate_mn}},
#' \code{\link{lsm_c_gyrate_sd}},
#' \code{\link{lsm_c_gyrate_cv}}, \cr
#' \code{\link{lsm_l_gyrate_mn}},
#' \code{\link{lsm_l_gyrate_sd}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_gyrate_cv(landscape)
#'
#' @aliases lsm_l_gyrate_cv
#' @rdname lsm_l_gyrate_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_gyrate_cv <- function(landscape) UseMethod("lsm_l_gyrate_cv")

#' @name lsm_l_gyrate_cv
#' @export
lsm_l_gyrate_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_gyrate_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_gyrate_cv
#' @export
lsm_l_gyrate_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_gyrate_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_gyrate_cv
#' @export
lsm_l_gyrate_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_gyrate_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_gyrate_cv
#' @export
lsm_l_gyrate_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_gyrate_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_gyrate_cv_calc <- function(landscape) {

    gyrate_cv <- landscape %>%
        lsm_p_gyrate_calc() %>%
        dplyr::summarize(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "gyrate_cv",
        value = as.double(gyrate_cv$value)
    )

}

