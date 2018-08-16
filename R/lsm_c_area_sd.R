#' AREA_SD (class level)
#'
#' @description Standard deviation of patch area (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA_{SD} = sd(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares.
#'
#' AREA_SD is an 'Area and Edge metric'. The metric summarises each class
#' as the standard deviation of all patch areas belonging to class i.
#' The metric describes the differences among patches of the same class i in
#' the landscape.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_SD >= 0}
#' \subsection{Behaviour}{Equals AREA_SD = 0 if all patches are identical in size.
#' Increases, without limit, as the variation of patch areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_area_mn}},
#' \code{\link{lsm_c_area_cv}}, \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_sd}},
#' \code{\link{lsm_l_area_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_area_sd(landscape)
#'
#' @aliases lsm_c_area_sd
#' @rdname lsm_c_area_sd
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_area_sd <- function(landscape, directions) UseMethod("lsm_c_area_sd")

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_sd_calc,
                   directions = directions,  .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_sd
#' @export
lsm_c_area_sd.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_c_area_sd_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_area_sd_calc <- function(landscape, directions){
    area_sd <- landscape %>%
        lsm_p_area_calc(directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = stats::sd(value))

    tibble::tibble(
        level = "class",
        class = as.integer(area_sd$class),
        id = as.integer(NA),
        metric = "area_sd",
        value = as.double(area_sd$value)
    )
}
