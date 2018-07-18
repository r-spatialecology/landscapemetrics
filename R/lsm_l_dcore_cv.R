#' DCORE_CV (landscape level)
#'
#' @description Coeffiecent of variation number of disjunct core areas (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{DCORE_{CV} = cv(NCORE[patch_{ij}])}
#' where \eqn{NCORE[patch_{ij}]} is the number of core areas.
#'
#' DCORE_CV is an 'Core area metric'. It summarises the landscape as the coeffiecent
#' of variation of all patches belonging to the landscape. A cell is defined as core if
#' the cell has no neighbour with a different value than itself (rook's case). NCORE counts
#' the disjunct core areas, whereby a core area is a 'patch within the patch' containing
#' only core cells. The metric describes the differences among all patches in the landscape
#' and is easily comparable because it is scaled to the mean.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{DCORE_CV >= 0}
#' \subsection{Behaviour}{Equals DCORE_CV = 0 if all patches have the same number of disjunct
#' core areas. Increases, without limit, as the variation of number of disjunct corea areas
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_nca}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_dcore_mn}},
#' \code{\link{lsm_c_dcore_sd}},
#' \code{\link{lsm_c_dcore_cv}}, \cr
#' \code{\link{lsm_l_dcore_mn}},
#' \code{\link{lsm_l_dcore_sd}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_dcore_cv(landscape)
#'
#' @aliases lsm_l_dcore_cv
#' @rdname lsm_l_dcore_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_dcore_cv <- function(landscape) UseMethod("lsm_l_dcore_cv")

#' @name lsm_l_dcore_cv
#' @export
lsm_l_dcore_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_dcore_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_dcore_cv
#' @export
lsm_l_dcore_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_dcore_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_dcore_cv
#' @export
lsm_l_dcore_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_dcore_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_dcore_cv
#' @export
lsm_l_dcore_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_dcore_cv_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_dcore_cv_calc <- function(landscape){

    dcore_cv <- landscape %>%
        lsm_p_nca_calc() %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "dcore_cv",
        value = as.double(dcore_cv$value)
    )
}
