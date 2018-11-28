#' AREA_CV (landscape level)
#'
#' @description Coefficient of variation of patch area (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA_{CV} = cv(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares.
#'
#' AREA_CV is an 'Area and Edge metric'. The metric summarises the landscape
#' as the Coefficient of variation of all patches in the landscape.
#' The metric describes the differences among patches in the landscape and is
#' easily comparable because it is scaled to the mean.
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
#' \code{\link{lsm_c_area_sd}},
#' \code{\link{lsm_c_area_cv}}, \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_sd}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_area_cv(landscape)
#'
#' @aliases lsm_l_area_cv
#' @rdname lsm_l_area_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_area_cv <- function(landscape, directions) UseMethod("lsm_l_area_cv")

#' @name lsm_l_area_cv
#' @export
lsm_l_area_cv.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_area_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_area_cv
#' @export
lsm_l_area_cv.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_area_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_area_cv
#' @export
lsm_l_area_cv.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_area_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_area_cv
#' @export
lsm_l_area_cv.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_area_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_area_cv
#' @export
lsm_l_area_cv.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_area_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_l_area_cv_calc <- function(landscape, directions){

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions)

    # calculate cv
     area_cv <- dplyr::summarise(area_patch, value = raster::cv(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "area_cv",
        value = as.double(area_cv$value)
    )
}


