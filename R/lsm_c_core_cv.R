#' CORE_CV (class level)
#'
#' @description Coefficient of variation of core area (Core area metric)
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#'
#' @details
#' \deqn{CORE_{CV} = cv(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_CV is a 'Core area metric'. It equals the Coefficient of variation of the core area
#' of each patch belonging to class i. The core area is defined as all cells that have no
#' neighbour with a different value than themselves (rook's case). The metric describes the
#' differences among patches of the same class i in the landscape and is easily comparable
#' because it is scaled to the mean.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE_CV >= 0}
#' \subsection{Behaviour}{Equals CORE_CV = 0 if all patches have the same core area.
#' Increases, without limit, as the variation of patch core areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_sd}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_sd}},
#' \code{\link{lsm_l_core_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_core_cv(landscape)
#'
#' @aliases lsm_c_core_cv
#' @rdname lsm_c_core_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_core_cv <- function(landscape, directions, consider_boundary) UseMethod("lsm_c_core_cv")

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.RasterLayer <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_core_cv_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.RasterStack <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_core_cv_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.RasterBrick <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_core_cv_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.stars <- function(landscape, directions = 8, consider_boundary = FALSE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_core_cv_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.list <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_core_cv_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_c_core_cv_calc <- function(landscape, directions, consider_boundary){

    core <- lsm_p_core_calc(landscape,
                            directions = directions,
                            consider_boundary = consider_boundary)

    core_cv <- dplyr::summarise(dplyr::group_by(core, class),
                                value = raster::cv(value))

    tibble::tibble(
        level = "class",
        class = as.integer(core_cv$class),
        id = as.integer(NA),
        metric = "core_cv",
        value = as.double(core_cv$value)
    )
}
