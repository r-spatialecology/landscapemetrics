#' DCAD (class level)
#
#' @description Disjunct core area density (Corea area metric)
#' @param directions The number of directions in which cells should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.

#' @details
#' \deqn{DCAD = (\frac{\sum \limits_{j = 1}^{n} n_{ij}^{core}} {A}) * 10000 * 100}
#' where \eqn{n_{ij}^{core}} is the number of disjunct core areas and \eqn{A}
#' is the total landscape area in square meters.
#'
#' DCAD is a 'Core area metric'. It equals the number of disjunct core areas per
#' 100 ha relative to the total area. A disjunct core area is a 'patch within
#' the patch' containing only core cells. A cell is defined as core area if the cell has no
#' neighbour with a different value than itself (rook's case). The metric is relative and
#' therefore comparable among landscapes with different total areas.
#'
#' \subsection{Units}{Number per 100 hectares}
#' \subsection{Range}{DCAD >= 0}
#' \subsection{Behaviour}{Equals DCAD = 0 when DCORE = 0, i.e. no patch of class i contains
#' a disjunct core area. Increases, without limit, as disjunct core areas become more
#' present, i.e. patches becoming larger and less complex.}
#'
#' @seealso
#' \code{\link{lsm_c_ndca}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_dcad}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_dcad(landscape)
#'
#' @aliases lsm_c_dcad
#' @rdname lsm_c_dcad
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_dcad <- function(landscape, directions) UseMethod("lsm_c_dcad")

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_dcad_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_dcad_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_dcad_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_dcad
#' @export
lsm_c_dcad.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_dcad_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_dcad_calc <- function(landscapedirections){

    area_landscape <- lsm_l_ta_calc(landscape, directions = directions)

    ndca_class <- lsm_c_ndca_calc(landscape, directions = directions)

    dcad <- dplyr::mutate(ndca_class,
                          value = (value / area_landscape$value) * 100)

    tibble::tibble(
        level = "class",
        class = as.integer(dcad$class),
        id = as.integer(NA),
        metric = "dcad",
        value = as.double(dcad$value)
    )
}
