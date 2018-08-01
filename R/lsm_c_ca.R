#' CA (class level)
#'
#' @description Total (class) area (Area and edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CA = sum(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares.
#'
#' CA is an 'Area and egdge metric' and a measure of composition.
#' The total (class) area sums the area of all patches belonging to class i.
#' It shows if the landcsape is e.g. dominated by one class or if all classes
#' are equally present. CA is an absolute measure, making comparisons among landscapes with different
#' total areas difficult.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CA > 0}
#' \subsection{Behaviour}{Aprroaches CA > 0 as the patch areas of class i become small.
#' Increases, without limit, as the patch areas of class i become large. CA = TA if only
#' one class is present.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{sum}}, \cr
#' \code{\link{lsm_l_ta}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ca(landscape)
#'
#' @aliases lsm_c_ca
#' @rdname lsm_c_ca
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_ca <- function(landscape, directions) UseMethod("lsm_c_ca")

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape, lsm_c_ca_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_ca_calc <- function(landscape, directions) {
    total_area <- landscape %>%
        lsm_p_area_calc(., directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value))

    tibble::tibble(
        level = "class",
        class = as.integer(total_area$class),
        id = as.integer(NA),
        metric = "ca",
        value = as.double(total_area$value)
    )
}
