#' AREA_MN (class level)
#'
#' @description Mean of patch area (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{AREA_{MN} = mean(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares
#'
#' AREA_MN is an 'Area and Edge metric'. The metric summarises each class
#' as the mean of all patch areas belonging to class i. The metric is a simple way
#' to describe the composition of the landscape. Especially together with the total
#' class area (\code{\link{lsm_c_ca}}), it can also give an an idea of patch structure
#' (e.g. many small patches vs. few larges patches).
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{AREA_MN > 0}
#' \subsection{Behaviour}{Approaches AREA_MN = 0 if all patches are small. Increases, without
#' limit, as the patch areas increase.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_area_cv}},
#' \code{\link{lsm_c_area_sd}}, \cr
#' \code{\link{lsm_l_area_mn}},
#' \code{\link{lsm_l_area_sd}},
#' \code{\link{lsm_l_area_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_area_mn(landscape)
#'
#' @aliases lsm_c_area_mn
#' @rdname lsm_c_area_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_area_mn <- function(landscape, directions) UseMethod("lsm_c_area_mn")

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_mn_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_mn_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_mn_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_area_mn_calc,
                   directions = directions,  .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}


#' @name lsm_c_area_mn
#' @export
lsm_c_area_mn.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_c_area_mn_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_area_mn_calc <- function(landscape, directions){

    area_mean <- landscape %>%
        lsm_p_area_calc(., directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value))

    tibble::tibble(
        level = "class",
        class = as.integer(area_mean$class),
        id = as.integer(NA),
        metric = "area_mn",
        value = as.double(area_mean$value)
    )
}
