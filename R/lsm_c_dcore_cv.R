#' DCORE_CV (class level)
#'
#' @description Coeffiecent of variation number of disjunct core areas (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{DCORE_{CV} = cv(NCORE[patch_{ij}])}
#' where \eqn{NCORE[patch_{ij}]} is the number of core areas.
#'
#' DCORE_CV is an 'Core area metric'. It summarises each class as the coeffiecent
#' of variation of all patch areas belonging to class i. A cell is defined as core if the cell
#' has no neighbour with a different value than itself (rook's case). NCORE counts the disjunct
#' core areas, whereby a core area is a 'patch within the patch' containing only core cells.
#' The metric describes the differences among patches of the same class i in
#' the landscape and is easily comparable because it is scaled to the mean.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{DCORE_CV >= 0}
#' \subsection{Behaviour}{Equals DCORE_CV = 0 if all patches have the same number of disjunct
#' core areas. Increases, without limit, as the variation of number of disjunct corea areas
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_ncore}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_dcore_mn}},
#' \code{\link{lsm_c_dcore_sd}}, \cr
#' \code{\link{lsm_l_dcore_mn}},
#' \code{\link{lsm_l_dcore_sd}},
#' \code{\link{lsm_l_dcore_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_dcore_cv(landscape)
#'
#' @aliases lsm_c_dcore_cv
#' @rdname lsm_c_dcore_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_dcore_cv <- function(landscape) UseMethod("lsm_c_dcore_cv")

#' @name lsm_c_dcore_cv
#' @export
lsm_c_dcore_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcore_cv_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcore_cv
#' @export
lsm_c_dcore_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcore_cv_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_dcore_cv
#' @export
lsm_c_dcore_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_dcore_cv_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_dcore_cv
#' @export
lsm_c_dcore_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_dcore_cv_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_dcore_cv_calc <- function(landscape){
    dcore_sd <- landscape %>%
        lsm_p_ncore_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(dcore_sd$class),
        id = as.integer(NA),
        metric = "number of core areas (cv)",
        value = as.double(dcore_sd$value)
    )
}
