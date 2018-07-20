#' AREA_CV (class level)
#'
#' @description Coeffiecent of variation of patch area (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers
#' @param directions The number of directions in which cells should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA_{CV} = cv(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares.
#'
#' AREA_CV is an 'Area and Edge metric'. The metric summarises each class
#' as the coeffiecent of variation of all patch areas belonging to class i.
#' The metric describes the differences among patches of the same class i in
#' the landscape and is easily comparable because it is scaled to the mean.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_CV >= 0}
#' \subsection{Behaviour}{Equals AREA_CV = 0 if all patches are identical in size.
#' Increases, without limit, as the variation of patch areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_area_mn}},
#' \code{\link{lsm_c_area_sd}}, \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_sd}},
#' \code{\link{lsm_l_area_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_area_cv(landscape)
#'
#' @aliases lsm_c_area_cv
#' @rdname lsm_c_area_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_area_cv <- function(landscape, direction) UseMethod("lsm_c_area_cv")

#' @name lsm_c_area_cv
#' @export
lsm_c_area_cv.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_cv_calc, directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_area_cv
#' @export
lsm_c_area_cv.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_cv_calc,
                   directions = directions,  .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_cv
#' @export
lsm_c_area_cv.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_cv_calc,
                   directions = directions,  .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_cv
#' @export
lsm_c_area_cv.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_c_area_cv_calc,
                   directions = directions,  .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_area_cv_calc <- function(landscape, directions){
    area_cv <- landscape %>%
        lsm_p_area_calc(.,directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(area_cv$class),
        id = as.integer(NA),
        metric = "area_cv",
        value = as.double(area_cv$value)
    )
}
