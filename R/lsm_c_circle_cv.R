#' CIRCLE_CV (Class level)
#'
#' @description Coeffiecient of variation of related circumscribing circle (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CIRCLE_{CV} = cv(CIRCLE[patch_{ij}])}
#' where \eqn{CIRCLE[patch_{ij}]} is the related circumscribing circle of each patch.
#'
#' CIRCLE_CV is a 'Shape metric' and summarises each class as the coeffiecent of variation of
#' the related circumscribing circle of all patches belonging to class i. CIRCLE describes
#' the ratio between the patch area and the smallest circumscribing circle of the patch
#' and characterises the compactness of the patch. CIRCLE_CV describes the differences among
#' patches of the same class i in the landscape. Because it is scaled to the mean,
#' it is easily comparable.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CIRCLE_CV >= 0}
#' \subsection{Behaviour}{Equals CIRCLE_CV if the related circumscribing circle is identical
#' for all patches. Increases, without limit, as the variation of related circumscribing
#' circles increases.}
#'
#' @seealso
#' \code{\link{lsm_p_circle}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_circle_mn}},
#' \code{\link{lsm_c_circle_sd}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_sd}},
#' \code{\link{lsm_l_circle_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_circle_cv(landscape)
#'
#' @aliases lsm_c_circle_cv
#' @rdname lsm_c_circle_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_circle_cv <- function(landscape) UseMethod("lsm_c_circle_cv")

#' @name lsm_c_circle_cv
#' @export
lsm_c_circle_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_circle_cv
#' @export
lsm_c_circle_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_circle_cv
#' @export
lsm_c_circle_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_circle_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_circle_cv
#' @export
lsm_c_circle_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_circle_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_circle_cv_calc <- function(landscape) {

    circle_mn  <- landscape %>%
        lsm_p_circle_calc() %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(circle_mn$class),
        id = as.integer(NA),
        metric = "related circumscribing circle (mean)",
        value = as.double(circle_mn$value)
    )

}

