#' CORE_SD (landscape level)
#'
#' @description Standard deviation of patch core area (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#'
#' @details
#' \deqn{CORE_{SD} = sd(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_SD is a 'Core area metric'. It equals the standard deviation of the core area
#' of all patches in the landscape. The core area is defined as all cells that have no
#' neighbour with a different value than themselves (rook's case). The metric describes the
#' differences among all patches in the landscape.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE_SD >= 0}
#' \subsection{Behaviour}{Equals CORE_SD = 0 if all patches have the same core area.
#' Increases, without limit, as the variation of patch core areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_sd}},
#' \code{\link{lsm_c_core_cv}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_core_sd(landscape)
#'
#' @aliases lsm_l_core_sd
#' @rdname lsm_l_core_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_core_sd <- function(landscape, directions, consider_boundary) UseMethod("lsm_l_core_sd")

#' @name lsm_l_core_sd
#' @export
lsm_l_core_sd.RasterLayer <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_core_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_core_sd
#' @export
lsm_l_core_sd.RasterStack <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_core_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_core_sd
#' @export
lsm_l_core_sd.RasterBrick <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_core_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_core_sd
#' @export
lsm_l_core_sd.stars <- function(landscape, directions = 8, consider_boundary = FALSE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_core_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_core_sd
#' @export
lsm_l_core_sd.list <- function(landscape, directions = 8, consider_boundary = FALSE) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_core_sd_calc,
                     directions = directions,
                     consider_boundary = consider_boundary)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_l_core_sd_calc <- function(landscape, directions, consider_boundary){

    core_sd <- dplyr::summarise(lsm_p_core_calc(landscape,
                                                directions = directions,
                                                consider_boundary = consider_boundary),
                                value = stats::sd(value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "core_sd",
        value = as.double(core_sd$value)
    )
}
