#' CAI_CV (class level)
#'
#' @description Coefficient of variation of core area index (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CAI_{CV} = cv(CAI[patch_{ij}]}
#' where \eqn{CAI[patch_{ij}]} is the core area index of each patch.
#'
#' CAI_CV is a 'Core area metric'. The metric summarises each class
#' as the Coefficient of variation of the core area index of all patches
#' belonging to class i. The core area index is the percentage of core area
#' in relation to patch area. A cell is defined as core area if the cell has
#' no neighbour with a different value than itself (rook's case). The metric
#' describes the differences among patches of the same class i in
#' the landscape. Because it is scaled to the mean, it is easily comparable.
#'
#' \subsection{Units}{Percent}
#' \subsection{Range}{CAI_CV >= 0}
#' \subsection{Behaviour}{Equals CAI_CV = 0 if the core area index is identical
#' for all patches. Increases, without limit, as the variation of the core area
#' indices increases.}
#'
#' @seealso
#' \code{\link{lsm_p_cai}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_cai_mn}},
#' \code{\link{lsm_c_cai_sd}}, \cr
#' \code{\link{lsm_l_cai_mn}},
#' \code{\link{lsm_l_cai_sd}},
#' \code{\link{lsm_l_cai_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cai_cv(landscape)
#'
#' @aliases lsm_c_cai_cv
#' @rdname lsm_c_cai_cv
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_cai_cv <- function(landscape, directions) UseMethod("lsm_c_cai_cv")

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_c_cai_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_c_cai_cv
#' @export
lsm_c_cai_cv.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_c_cai_cv_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_c_cai_cv_calc <- function(landscape, directions){

    cai <- lsm_p_cai_calc(landscape, directions = directions)

    cai_cv <- dplyr::summarise(dplyr::group_by(cai, class),
                               value = raster::cv(value))

    tibble::tibble(
        level = "class",
        class = as.integer(cai_cv$class),
        id = as.integer(NA),
        metric = "cai_cv",
        value = as.double(cai_cv$value)
    )
}
